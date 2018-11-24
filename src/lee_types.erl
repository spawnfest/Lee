-module(lee_types).

%% API exports
-export([ union/2
        , boolean/0
        , validate_union/3

        , term/0
        , any/0
        , validate_term/3

        , integer/0
        , non_neg_integer/0
        , range/2
        , validate_integer/3

        , float/0
        , validate_float/3

        , atom/0
        , validate_atom/3

        , binary/0
        , validate_binary/3

        , list/0
        , list/1
        , non_empty_list/1
        , string/0
        , validate_list/3


        , tuple/0
        , validate_any_tuple/3

        , tuple/1
        , validate_tuple/3

        , map/2
        , validate_map/3

        , exact_map/1
        , validate_exact_map/3

        , number/0

        , print_type/1
        ]).

-type validate_result() :: ok | {error, term()}.

%%====================================================================
%% Macros
%%====================================================================

-define(te(Name, Attrs, Parameters), { [lee, base_types, Name]
                                     , Attrs
                                     , Parameters
                                     }).
-define(te(Attrs, Parameters), ?te(?FUNCTION_NAME, Attrs, Parameters)).
-define(te(Parameters), ?te(?FUNCTION_NAME, #{}, Parameters)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================
-spec union(lee:typedef(), lee:typedef()) -> lee:typedef().
union(A, B) ->
    ?te([A, B]).

-spec validate_union( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_union(Model, {_, _, [A, B]}, Term) ->
    case lee:validate_term(Model, A, Term) of
        ok ->
            ok;
        {error, _} ->
            case lee:validate_term(Model, B, Term) of
                ok ->
                    ok;
                {error, _} ->
                    Msg = format( "Expected ~s | ~s, got ~p"
                                , [print_type(A), print_type(B), Term]
                                ),
                    {error, Msg}
            end
    end.

-spec boolean() -> lee:typedef().
boolean() ->
    union(true, false).

-spec integer() -> lee:typedef().
integer() ->
    range(neg_infinity, infinity).

-spec validate_integer( lee:model_fragment()
                      , lee:typedef()
                      , term()
                      ) -> validate_result().
validate_integer(_, Self = {_, #{range := {A, B}}, []}, Term) ->
    try
        is_integer(Term) orelse throw(badint),
        A =:= neg_infinity orelse Term >= A orelse throw(badint),
        B =:= infinity     orelse Term =< B orelse throw(badint),
        ok
    catch
        badint ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Self), Term]
                          )}
    end.

-spec non_neg_integer() -> lee:typedef().
non_neg_integer() ->
    range(0, infinity).

-spec range( integer() | neg_infinity
           , integer() | infinity
           ) -> lee:typedef().
range(A, B) ->
    ?te( integer
       , #{range => {A, B}}
       , []
       ).

-spec string() -> lee:typedef().
string() ->
    list(range(0, 16#10ffff)).

-spec float() -> lee:typedef().
float() ->
    ?te([]).

-spec validate_float( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_float(_, _, Term) ->
    if is_float(Term) ->
            ok;
       true ->
            {error, format("Expected float(), got ~p", [Term])}
    end.

-spec atom() -> lee:typedef().
atom() ->
    ?te([]).

-spec validate_atom( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_atom(_, _, Term) ->
    if is_atom(Term) ->
            ok;
       true ->
            {error, format("Expected atom(), got ~p", [Term])}
    end.

-spec binary() -> lee:typedef().
binary() ->
    ?te([]).

-spec validate_binary( lee:model_fragment()
                     , lee:typedef()
                     , term()
                     ) -> validate_result().
validate_binary(_, _, Term) ->
    if is_binary(Term) ->
            ok;
       true ->
            {error, format("Expected binary(), got ~p", [Term])}
    end.

-spec tuple() -> lee:typedef().
tuple() ->
    ?te(any_tuple, #{}, []).

-spec validate_any_tuple( lee:model_fragment()
                        , lee:typedef()
                        , term()
                        ) -> validate_result().
validate_any_tuple(_, _, Term) ->
    if is_tuple(Term) ->
            ok;
       true ->
            {error, format("Expected tuple(), got ~p", [Term])}
    end.

-spec tuple([lee:typedef()]) -> lee:typedef().
tuple(Params) ->
    ?te(Params).

-spec validate_tuple( lee:model_fragment()
                    , lee:typedef()
                    , term()
                    ) -> validate_result().
validate_tuple(Model, Self = {_, _, Params}, Term) ->
    try
        is_tuple(Term)
            orelse throw(badtuple),
        List = tuple_to_list(Term),
        length(Params) =:= length(List)
            orelse throw(badtuple),
        lists:zipwith( fun(Type, Val) ->
                               %% TODO: make better error message
                               lee:validate_term(Model, Type, Val) =:= ok
                                   orelse throw(badtuple)
                       end
                     , Params
                     , List
                     ),
        ok
    catch
        badtuple ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Self), Term]
                          )}
    end.

-spec term() -> lee:typedef().
term() ->
    ?te([]).

-spec any() -> lee:typedef().
any() ->
    term().

-spec validate_term( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_term(_, _, _) ->
    ok.

-spec list() -> lee:typedef().
list() ->
    list(term()).

-spec list(lee:typedef()) -> lee:typedef().
list(Type) ->
    ?te(#{non_empty => false}, [Type]).

-spec non_empty_list(lee:typedef()) -> lee:typedef().
non_empty_list(Type) ->
    ?te(list, #{non_empty => true}, [Type]).

-spec validate_list( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_list( Model
             , Self = {_, #{non_empty := NonEmpty}, [Param]}
             , Term
             ) ->
    try
        is_list(Term) orelse throw(badlist),
        not(NonEmpty) orelse length(Term) > 0 orelse throw(badlist),
        validate_list_(Model, Param, Term),
        ok
    catch
        {badelem, Elem} ->
            {error, format( "Expected ~s, got ~p in ~s"
                          , [print_type(Param), Elem, print_type(Self)]
                          )};
        badlist ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Self), Term]
                          )}
    end.

-spec map(lee:typedef(), lee:typedef()) -> lee:typedef().
map(K, V) ->
    ?te([K, V]).

-spec validate_map( lee:model_fragment()
                  , lee:typedef()
                  , term()
                  ) -> validate_result().
validate_map( Model
            , Self = {_, _, [KeyT, ValueT]}
            , Term
            ) ->
    try
        is_map(Term) orelse throw(badmap),
        [begin
             lee:validate_term(Model, KeyT, K) =:= ok
                 orelse throw(badmap),
             lee:validate_term(Model, ValueT, V) =:= ok
                 orelse throw({badval, K, V})
         end
         || {K, V} <- maps:to_list(Term)],
        ok
    catch
        {badval, Key, Val} ->
            {error, format( "Expected ~s, but key ~p got value ~p instead"
                          , [print_type(Self), Key, Val]
                          )};
        badmap ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Self), Term]
                          )}
    end.

%% "Literal" map
-spec exact_map(#{term() := lee:typedef()}) -> lee:typedef().
exact_map(Spec) ->
    ?te( #{ exact_map_spec => Spec
          , mandatory_map_fields => maps:keys(Spec)
          }
       , []
       ).

-spec validate_exact_map( lee:model_fragment()
                        , lee:typedef()
                        , term()
                        ) -> validate_result().
validate_exact_map( Model
                  , Self = {_, Attr, _}
                  , Term
                  ) ->
    #{ exact_map_spec := Spec
     , mandatory_map_fields := Mandatory0
     } = Attr,
    try
        is_map(Term) orelse throw(badmap),
        Mandatory = map_sets:from_list(Mandatory0),
        maps:map( fun(K, Type) ->
                          case {Term, map_sets:is_element(K, Mandatory)} of
                              {#{K := Val}, _} ->
                                  lee:validate_term(Model, Type, Val) =:= ok
                                      orelse throw({badval, K, Val, Type});
                              {_, true} ->
                                  throw({badkey, K});
                              {_, false} ->
                                  ok
                          end
                  end
                , Spec
                ),
        ok
    catch
        {badval, Key, Val, ValType} ->
            {error, format( "Expected ~s in key ~p of ~s, got ~p"
                          , [ print_type(ValType)
                            , Key
                            , print_type(Self)
                            , Val
                            ]
                          )};
        {badkey, Key} ->
            {error, format( "Missing key(s) ~p in ~s"
                          , [Term, print_type(Self)]
                          )};
        badmap ->
            {error, format( "Expected ~s, got ~p"
                          , [print_type(Self), Term]
                          )}
    end.

number() ->
    union(integer(), float()).

print_type(Type) ->
    %% TODO: stub
    io_lib:format("~p", [Type]).

%%====================================================================
%% Internal functions
%%====================================================================

format(Fmt, Attrs) ->
    lists:flatten(io_lib:format(Fmt, Attrs)).

validate_list_(_, _, []) ->
    ok;
validate_list_(Model, Param, [Term|Tail]) ->
    is_list(Tail) orelse throw(badlist),
    lee:validate_term(Model, Param, Term) =:= ok
        orelse throw({badelem, Term}),
    validate_list_(Model, Param, Tail).

%%====================================================================
%% Unit tests
%%====================================================================

%% Can't use PropER in this module

-ifdef(TEST).

-define(valid(Type, Term),
        ?assertMatch( ok
                    , lee:validate_term(lee:base_model(), Type, Term)
                    )).

-define(invalid(Type, Term),
        ?assertMatch( {error, _}
                    , lee:validate_term(lee:base_model(), Type, Term)
                    )).

validate_concrete_atom_test() ->
    ?valid(true, true),
    ?valid(false, false),
    ?invalid(foo, 1),
    ?invalid(foo, []),
    ?invalid(foo, bar).

validate_bool_test() ->
    ?valid(boolean(), true),
    ?valid(boolean(), false),
    ?invalid(boolean(), 1),
    ?invalid(boolean(), {}),
    ?invalid(boolean(), foo).

integer_test() ->
    ?valid(integer(), -1),
    ?valid(integer(), 1000),
    ?valid(integer(), 0),
    ?invalid(integer(), 1.0),
    ?invalid(integer(), foo),
    ?valid(range(-1, 1), 1),
    ?valid(range(-1, 1), -1),
    ?invalid(range(-1, 1), -2),
    ?invalid(range(-1, 1), 2),
    ?valid(non_neg_integer(), 0),
    ?valid(non_neg_integer(), 1),
    ?invalid(non_neg_integer(), -1).

union_test() ->
    ?valid(number(), 1),
    ?valid(number(), 1.1),
    ?invalid(number(), []).

term_test() ->
    ?valid(term(), 1),
    ?valid(term(), 1.1),
    ?valid(term(), {1, 2, [], foo}),
    ?valid(term(), [foo, 1, [] | gg]).

atom_test() ->
    ?valid(atom(), foo),
    ?valid(atom(), bar),
    ?invalid(atom(), {}),
    ?invalid(atom(), 1).

list_test() ->
    ?valid(list(), []),
    ?valid(non_empty_list(integer()), [1, 2, 3]),
    ?invalid(non_empty_list(term()), []),
    UnionL = list(union(boolean(), integer())),
    ?valid(UnionL, [true, false, 1, 10, -1]),
    ?invalid(UnionL, [true, false, 1, bar]),
    ?invalid(list(), [foo, bar | baz]).

string_test() ->
    ?valid(string(), "this is a string"),
    ?valid(string(), "(✿ ┛O‿‿O)┛彡♥   ¯\_(ツ)_/¯"),
    ?invalid(string(), "foo" ++ [bar, {}] ++ "baz"),
    ?invalid(string(), [-1, 2]).

tuple_test() ->
    ?valid(tuple(), {}),
    ?valid(tuple(), {foo, 1, []}),
    ?invalid(tuple(), 1),
    ?invalid(tuple(), []),

    ?valid(tuple([]), {}),
    ?invalid(tuple([]), {1}),

    T = tuple([atom(), integer()]),
    ?valid(T, {foo, 1}),
    ?valid(T, {false, -1}),
    ?invalid(T, {false, -1, 5}),
    ?invalid(T, {false}),
    ?invalid(T, {false, "1"}).

binary_test() ->
    ?valid(binary(), <<>>),
    ?valid(binary(), <<"foo">>),
    ?invalid(binary(), "fooo"),
    ?invalid(binary(), 1).

map_test() ->
    T = map(atom(), string()),
    ?valid(T, #{}),
    ?valid(T, #{foo => "bar"}),
    ?invalid(T, #{ foo => "bar"
                 , "bar" => foo
                 , baz => "quux"
                 }),
    ?invalid(T, #{ foo => 1
                 , bar => "bar"
                 }).

exact_map_test() ->
    T = exact_map(#{ foo => boolean()
                   , bar => string()
                   }),
    ?valid(T, #{foo => true, bar => "bar"}),
    ?valid(T, #{foo => false, bar => []}),
    ?invalid(T, #{foo => foo, bar => "bar"}),
    ?invalid(T, #{foo => true}),
    ?invalid(T, #{foo => true, bar => 1}).

-endif.

-module(lee_transform).

-export([parse_transform/2]).

%% TODO: Source code locations are... approximate
%% TODO: Error handling is inexistent
%% TODO: Parse `-export_type' attribute and export lee types automatically

-type local_tref() :: {Name :: atom(), Arity :: integer()}.

-type ast() :: term().

-type ast_var() :: term().

-record(s,
        { local_types     :: #{local_tref() => {ast(), [ast()]}}
        , reflected_types :: #{local_tref() => {Namespace :: lee:key(), AST :: ast()}}
        , custom_verif    :: #{local_tref() => ast()}
        , line            :: integer() | undefined
        , namespace       :: lee:key()
        }).

-define(INT(Line, Val),
        {integer, Line, Val}).

-define(INT(Val),
        ?INT(_, Val)).

-define(ATOM(Line, Atom),
        {atom, Line, Atom}).

-define(ATOM(Atom),
        ?ATOM(_, Atom)).

-define(LCALL(Line, Name, Args),
        {call, Line, ?ATOM(Name), Args}).

-define(MK_LCALL(Line, Name, Args),
        {call, Line, ?ATOM(Line, Name), Args}).

-define(RCALL(Line, Module, Function, Args),
        {call, Line
        , {remote, _, ?ATOM(Module), ?ATOM(Function)}
        , Args
        }).

-define(MK_RCALL(Line, Module, Function, Args),
        {call, Line
        , {remote, Line, ?ATOM(Line, Module), ?ATOM(Line, Function)}
        , Args
        }).

-define(MK_TYPEDEF(Line, Name, Arity, AST),
        {map_field_assoc, Line
        , {tuple, Line, [?ATOM(Line, Name), ?INT(Line, Arity)]}
        , AST
        }).

-define(LTYPE_REF(Name, Arity),
        {op, _, '/', ?ATOM(Name), {integer, _, Arity}}).

parse_transform(Forms0, _Options) ->
    Ignored = ignored(Forms0),
    CustomVerif = custom_verify(Forms0),
    Typedefs0 = local_typedefs(Forms0),
    Typedefs = maps:without(Ignored, Typedefs0),
    io:format( "~p~nIgnored: ~p~nCustom: ~p~nTypes ~p~n"
             , [Forms0, Ignored, CustomVerif, Typedefs]
             ),
    State0 = #s{ local_types = Typedefs
               , custom_verif = CustomVerif
               , reflected_types = #{}
               },
    {Forms1, State} = forms(Forms0, State0),
    Forms1.

forms(?RCALL(Line, lee, type_refl, [Namespace0, Types0]), State0) ->
    Namespace = literal_list(fun(?ATOM(A)) -> A end, Namespace0),
    State1 = State0#s{ line = Line
                     , namespace = Namespace
                     },
    Types1 = literal_list( fun(?LTYPE_REF(Name, Arity)) ->
                                   {Name, Arity}
                           end
                         , Types0
                         ),
    State2 = lists:foldl(fun mk_lee_type/2, State1, Types1),
    #s{reflected_types = RTypes} = State2,
    io:format("State: ~p~n", [State2]),
    TypesAST = [?MK_TYPEDEF(Line, Name, Arity, AST)
                || {{Name, Arity}, {Namespace1, AST}} <- maps:to_list(RTypes)
                 , Namespace1 =:= Namespace
               ],
    AST = ?MK_RCALL( Line
                   , lee, namespace
                   , [ Namespace0
                     , {map, Line, TypesAST}
                     ]
                   ),
    State = State2,
    {AST, State};
forms(L, State0) when is_list(L) ->
    {AST, State} = lists:mapfoldl(fun forms/2, State0, L);
forms(T, State0) when is_tuple(T) ->
    L = tuple_to_list(T),
    {AST, State} = forms(L, State0),
    {list_to_tuple(AST), State};
forms(AST, State) ->
    {AST, State}.

ignored(Forms) ->
    DeepDefs = [Defs || {attribute, _, lee_ignore, Defs} <- Forms],
    lists:usort(lists:append(DeepDefs)).

custom_verify(Forms) ->
    Defs = [Def || {attribute, _, lee_verify, Def} <- Forms],
    maps:from_list(Defs).

local_typedefs(Forms) ->
    maps:from_list([{{Name, length(Params)}, {AST, Params}}
                    || { attribute
                       , _Line
                       , type
                       , {Name, AST, Params}
                       } <- Forms
                   ]).

-spec mk_lee_type(local_tref(), #s{}) ->
                         #s{}.
mk_lee_type(Type, State0) ->
    #s{ local_types = #{Type := {AST0, Params}}
      , line = Line
      , namespace = Namespace
      } = State0,
    VarVals = do_type_vars(Line, Params),
    {AST, State1} = do_refl_type(State0, AST0, maps:from_list(VarVals)),
    #s{reflected_types = M0} = State1,
    Val = {Namespace, mk_type_alias(Line, Type, AST)},
    State1#s{reflected_types = M0 #{Type => Val}}.

-spec mk_type_alias(integer(), local_tref(), ast()) ->
                           #{local_tref() => ast()}.
mk_type_alias(Line, {Name, Arity}, AST) ->
    Variables = mk_literal_list( Line
                               , fun(I) -> ?INT(Line, I) end
                               , lists:seq(0, Arity - 1)
                               ),
    {tuple, Line
    , [ {cons, Line, ?ATOM(Line, typedef), {nil, Line}}
      , {map, Line,
         [ {map_field_assoc, Line
            , ?ATOM(Line, type)
            , AST
            }
          , {map_field_assoc, Line
            , ?ATOM(Line, type_variables)
            , Variables
            }
          ]}
       , {map, Line, []}
       ]
     }.

-spec check_local_type(local_tref(), #s{}) ->
                         #s{}.
check_local_type(TRef, State) ->
    %% FIXME:
    State.

%% Yay! The only place where line numbering is more or less correct!
-spec do_refl_type(#s{}, ast(), #{ast_var() => integer()}) ->
                          {ast(), #s{}}.
do_refl_type(State, {var, Line, Var}, VarVals) ->
    #{Var := N} = VarVals,
    AST = {tuple, Line, [ ?ATOM(Line, var)
                        , ?INT(Line, N)
                        ]},
    {AST, State};
do_refl_type(State, Int = ?INT(_, _), _) ->
    {Int, State};
do_refl_type(State, Atom = ?ATOM(_, _), _) ->
    {Atom, State};
do_refl_type(State0, _AST = {type, Line, Name, Args0}, VarVals) ->
    State1 = check_local_type({Name, length(Args0)}, State0),
    {Args1, State} = lists:mapfoldl( fun(I, S) ->
                                             do_refl_type(S, I, VarVals)
                                     end
                                   , State1
                                   , Args0
                                   ),
    case lists:member(Name, [tuple, union]) of
        true ->
            Args = [mk_literal_list( Line
                                   , fun(A) -> A end
                                   , Args1
                                   )];
        false ->
            Args = Args1
    end,
    {?MK_LCALL(Line, Name, Args), State};
do_refl_type(State0, AST0, VarVals) ->
    erlang:display({owo, AST0}),
    AST = ?ATOM(42, uguuuuu),
    {AST, State0}.

-spec do_type_vars(integer(), [ast()]) -> [{ast_var(), integer()}].
do_type_vars(Line, Params) ->
    {Result, _} =
        lists:mapfoldl( fun({var, _, Var}, N) ->
                                {{Var, N}, N + 1}
                        end
                      , 0
                      , Params
                      ),
    Result.

literal_list(_, {nil, _}) ->
    [];
literal_list(Fun, {cons, _, Val, Tail}) ->
    [Fun(Val) | literal_list(Fun, Tail)].

mk_literal_list(Line, _, []) ->
    {nil, Line};
mk_literal_list(Line, Fun, [Val|Tail]) ->
    {cons, Line, Fun(Val), mk_literal_list(Line, Fun, Tail)}.

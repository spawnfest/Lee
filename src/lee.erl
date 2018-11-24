-module(lee).

%% API exports
-export([ namespace/2
        , base_model/0
        , metametamodel/0
        , validate_term/3
        ]).

-export_type([ node/0
             , metatype/0
             , node_id/0
             , model_fragment/0
             , properties/0
             ]).

%%====================================================================
%% Types
%%====================================================================

-type validate_result() :: ok | {error, term()}.

-type metatype() :: atom().

-type node_id() :: atom().

-type key() :: [node_id()].

-type properties() :: #{atom() => term()}.

-type model_fragment() :: #{node_id() => moc() | model_fragment()}.

-type typedef() :: { Type       :: key()
                   , Attributes :: properties()
                   , Parameters :: [typedef()]
                   }
                 | atom() %% Literal atoms get a free pass
                 .

%% Managed object class
-type moc() :: {[metatype()], properties(), model_fragment()}
             | {[metatype()], properties()} %% Shortcut for child-free MOs
             .

%%====================================================================
%% Macros
%%====================================================================

-define(typedef(Name, Arity, Validate),
        Name => {[type]
                , #{validate => fun lee_types:Validate/3}
                , #{}
                }).

-define(typedef(Name, Validate), ?typedef(Name, 0, Validate)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Put model fragment in a namespace
-spec namespace(lee:key(), lee:model_fragment()) ->
                       lee:model_fragment().
namespace(Key, M) ->
    lists:foldl( fun(NodeId, Acc) ->
                         #{NodeId => Acc}
                 end
               , M
               , lists:reverse(Key)
               ).

%% @doc Model fragment containing base types.
-spec base_model() -> lee:model_fragment().
base_model() ->
    #{ lee =>
           #{ base_types =>
                  #{ ?typedef(union,              validate_union           )
                   , ?typedef(term,               validate_term            )
                   , ?typedef(integer,            validate_integer         )
                   , ?typedef(float,              validate_float           )
                   , ?typedef(atom,               validate_atom            )
                   , ?typedef(binary,             validate_binary          )
                   , ?typedef(any_tuple,          validate_any_tuple       )
                   , ?typedef(tuple,           1, validate_tuple           )
                   , ?typedef(list,            1, validate_list            )
                   , ?typedef(map,             2, validate_map             )
                   , ?typedef(exact_map,       2, validate_exact_map       )
                   }
            }
     }.

%% @doc A model validating metamodels
-spec metametamodel() -> lee:model_fragment().
metametamodel() ->
    MetaModel = #{
                 },
    {ok, Result} = lee_model:merge([MetaModel, base_model()]),
    Result.

-spec validate_term( lee:model_fragment()
                   , lee:typedef()
                   , term()
                   ) -> validate_result().
validate_term(Model, Atom, Term) when is_atom(Atom) ->
    case Term of
        Atom ->
            ok;
        _ ->
            {error, format( "Expected ~p, got ~p"
                          , [Atom, Term]
                          )}
    end;
validate_term(Model, Type = {TypeName, Attr, Params}, Term) ->
    {_, #{validate := Fun}, _} = lee_model:get(TypeName, Model),
    Fun(Model, Type, Term).

%%====================================================================
%% Internal functions
%%====================================================================

%% TODO get rid of duplicated functions and types
format(Fmt, Attrs) ->
    lists:flatten(io_lib:format(Fmt, Attrs)).


%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

-define(moc, {[], #{}, #{}}).

namespace_test() ->
    ?assertMatch( #{foo := #{bar := #{baz := #{}}}}
                , namespace([foo, bar], #{baz => #{}})
                ).

-endif.

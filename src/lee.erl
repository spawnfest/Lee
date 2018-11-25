-module(lee).

%% API exports
-export([ namespace/2
        , base_model/0
        , base_metamodel/0
        , metametamodel/0
        , validate_term/3
        , get/3
        , validate/2
        , validate_value/4
        , validate_command/4
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

-type node_id() :: atom()
                 | tuple()
                 .

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

-define(typedef(Name, Arity, Validate, Print),
        {Name, Arity} => {[type]
                         , #{ validate => fun lee_types:Validate/3
                            , print    => fun lee_types:Print/2
                            }
                         , #{}
                         }).

-define(typedef(Name, Validate, Print), ?typedef(Name, 0, Validate, Print)).

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
    namespace([lee, base_types]
             , #{ ?typedef(union,      2, validate_union,       print_union      )
                , ?typedef(term,          validate_term,        print_term       )
                , ?typedef(integer,       validate_integer,     print_integer    )
                , ?typedef(float,         validate_float,       print_float      )
                , ?typedef(atom,          validate_atom,        print_atom       )
                , ?typedef(binary,        validate_binary,      print_binary     )
                , ?typedef(tuple,         validate_any_tuple,   print_any_tuple  )
                , ?typedef(tuple,      1, validate_tuple,       print_tuple      )
                , ?typedef(list,       1, validate_list,        print_list       )
                , ?typedef(map,        2, validate_map,         print_map        )
                , ?typedef(exact_map,  1, validate_exact_map,   print_exact_map  )
                }
             ).

%% @doc Model fragment containing basic configuration validation
%% metaclasses
-spec base_metamodel() -> lee:model_fragment().
base_metamodel() ->
    namespace([lee, meta]
             , #{ value =>
                      {[metatype]
                      , #{meta_validate => fun validate_value/4}
                      , []
                      }
                , command =>
                      {[metatype]
                      , #{meta_validate => fun validate_command/4}
                      , []
                      }
                , valid_file =>
                      {[metatype]
                      , #{meta_validate => fun validate_valid_file/4}
                      , []
                      }
                }
             ).

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
validate_term(_Model, Atom, Term) when is_atom(Atom) ->
    case Term of
        Atom ->
            ok;
        _ ->
            {error, format( "Expected ~p, got ~p"
                          , [Atom, Term]
                          )}
    end;
validate_term(Model, Type = {TypeName, _Attr, Params}, Term) ->
    {Meta, Attr1, _} = lee_model:get(TypeName, Model),
    case {lists:member(type, Meta), lists:member(typedef, Meta)} of
        {true, false} ->
            #{validate := Fun} = Attr1,
            Fun(Model, Type, Term);
        {false, true} ->
            #{ type := Type1
             , type_variables := TypeVars
             } = Attr1,
            VarVals = maps:from_list(lists:zip(TypeVars, Params)),
            Type2 = subst_type_vars(Type1, VarVals),
            validate_term(Model, Type2, Term)
    end.

%% Get a `value' from the config:
-spec get(lee:model_fragment(), term(), lee:key()) ->
                 {ok, term()} | undefined.
get(Model, Config, Key) ->
    {_, #{getter := Getter}, _} = lee_model:get([lee, storage], Model),
    case Getter(Model, Config, Key) of
        {ok, Val} ->
            {ok, Val};
        undefined ->
            {MetaTypes, Attrs, _} = lee_model:get(Key, Model),
            case Attrs of
                #{default := Val} ->
                    {ok, Val};
                _ ->
                    undefined
            end
    end.

%% Validate the config against a model
-spec validate(lee:model_fragment(), term()) ->
                            {ok, Warnings :: [string()]}
                          | {error, Errors :: [string()], Warnings :: [string()]}
                          .
validate(Model, Config) ->
    ModelIdx = lee_model:mk_metatype_index(Model),
    {Errors, Warnings} =
        lists:foldl( fun({MT, MOCS}, {E0, W0}) ->
                             {E1, W1} = validate_mt_instances(Model, Config, MT, MOCS),
                             {E1 ++ E0, W1 ++ W0}
                     end
                   , {[], []}
                   , maps:to_list(ModelIdx)
                   ),
    case Errors of
        [] ->
            {ok, Warnings};
        _ ->
            {error, Errors, Warnings}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

validate_mt_instances(Model, Config, MetaTypeId, MOCS) ->
    try lee_model:get([lee, meta, MetaTypeId], Model) of
        MT = {[metatype], Attrs, []} ->
            Validate = maps:get(meta_validate, Attrs),
            GlobalValidate = maps:get(meta_global_validate, Attrs, fixme),
            lists:foldl( fun(MOCId, {E0, W0}) ->
                                 {E1, W1} = validate_moc_instances( Model
                                                                  , Config
                                                                  , Validate
                                                                  , MOCId
                                                                  ),
                                 {E1 ++ E0, W1 ++ W0}
                         end
                       , {[], []}
                       , map_sets:to_list(MOCS)
                       )
    catch _:_ ->
            %% TODO Ugly :(
            {[], []}
    end.

validate_moc_instances(Model, Config, Validate, MOCId) ->
    MOC = lee_model:get(MOCId, Model),
    case Validate(Model, Config, MOCId, MOC) of
        ok ->
            {[], []};
        {error, Err} ->
            {[Err], []}
    end.

validate_value(Model, Config, MOId, {_, Attrs, _}) ->
    {_, #{getter := Getter}, _} = lee_model:get([lee, storage], Model),
    Type = maps:get(type, Attrs),
    Mandatory = maps:get(mandatory, Attrs, false),
    case {Getter(Model, Config, MOId), Mandatory} of
        {{ok, Term}, _} ->
            validate_term(Model, Type, Term);
        {undefined, false} ->
            ok;
        {undefined, true} ->
            {error, format( "Mandatory value ~p is missing in the config"
                          , [MOId]
                          )}
    end.

validate_valid_file(Model, Config, MOId, _) ->
    {_, #{getter := Getter}, _} = lee_model:get([lee, storage], Model),
    case Getter(Model, Config, MOId) of
        {ok, Val} ->
            case io_lib:char_list(Val) andalso filelib:is_file(Val) of
                true ->
                    ok;
                false ->
                    {error, format("File ~p does not exist", [Val])}
            end;
        undefined ->
            ok
    end.

validate_command(Model, Config, MOCId, {_, Attrs, Params}) ->
    error(undefined).

%% TODO get rid of duplicated functions and types
format(Fmt, Attrs) ->
    lists:flatten(io_lib:format(Fmt, Attrs)).

subst_type_vars(Atom, _) when is_atom(Atom) ->
    Atom;
subst_type_vars({var, Var}, VarVals) ->
    #{Var := Subst} = VarVals,
    Subst;
subst_type_vars({Meta, Attr, Params}, VarVals) ->
    {Meta, Attr, [case I of
                      {var, Var} ->
                          #{Var := Subst} = VarVals,
                          Subst;
                      _ ->
                          subst_type_vars(I, VarVals)
                  end || I <- Params]}.

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

-define(moc, {[], #{}, #{}}).

namespace_test() ->
    ?assertMatch( #{foo := #{bar := #{baz := #{}}}}
                , namespace([foo, bar], #{baz => #{}})
                ).

-define(valid(Config),
        ?assertMatch( {ok, _}
                    , catch lee:validate(Model, Config)
                    )).

-define(invalid(Config),
        ?assertMatch( {error, _, _}
                    , catch lee:validate(Model, Config)
                    )).

validate_test() ->
    Model0 = #{ foo => {[value]
                       , #{mandatory => true, type => lee_types:boolean()}
                       }
              , bar => {[value]
                       , #{type => lee_types:integer()}
                       }
              },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  , lee_map_getter:model()
                                  , Model0
                                  ]),
    ?valid(#{[foo] => true}),
    ?valid(#{[foo] => true, [bar] => 1}),
    ?valid(#{[foo] => false, [bar] => -12}),
    ?invalid(#{}),
    ?invalid(#{[bar] => 1}),
    ?invalid(#{[foo] => 1}),
    ?invalid(#{[foo] => true, [bar] => 1.0}),
    ok.

get_test() ->
    Model0 = #{ foo => {[value]
                       , #{mandatory => true, type => lee_types:boolean()}
                       }
              , bar => {[value]
                       , #{type => lee_types:integer(), default => 42}
                       }
              },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  , lee_map_getter:model()
                                  , Model0
                                  ]),
    Config = #{ [foo] => true
              },
    ?assertMatch({ok, true}, lee:get(Model, Config, [foo])),
    ?assertMatch({ok, 42}, lee:get(Model, Config, [bar])),
    ok.


-endif.

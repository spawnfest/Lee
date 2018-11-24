-module(lee).

%% API exports
-export([ merge/1
        , merge/2
        , namespace/2
        , base_model/0
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

-type metatype() :: atom().

-type node_id() :: atom().

-type key() :: [node_id()].

-type properties() :: #{atom() => term()}.

-type model_fragment() :: #{node_id() => mo()}.

-type mo() :: {[metatype()], properties(), model_fragment()}
            | {[metatype()], properties()}     %% Shortcut for child-free MOs
            .

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Merge multiple model fragments while checking for clashing
%% names
-spec merge([lee:model_fragment()]) ->
                   {ok, lee:model_fragment()}
                 | {error, string()}
                 .
merge(FragList) ->
    lists:foldl(fun merge/2, #{}, FragList).

%% @doc Merge two model fragments while checking for clashing names
-spec merge(lee:model_fragment(), lee:model_fragment()) ->
                   {ok, lee:model_fragment()}
                 | {clashing_keys, [lee:node_id()]}
                 .
merge(M1, M2) ->
    M = maps:merge(M1, M2),
    OkSize = maps:size(M1) + maps:size(M2),
    case maps:size(M) of
        OkSize ->
            {ok, M};
        _ ->
            ClashingNames = map_sets:to_list(map_sets:union(M1, M2)),
            {clashing_keys, ClashingNames}
    end.

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

%% @doc Model fragment containing base types
-spec base_model() -> lee:model_fragment().
base_model() ->
    #{}.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).

merge_test() ->
    ?assertMatch( {ok, #{}}
                , merge(#{}, #{})
                ),
    ?assertMatch( {ok, #{foo := #{}, bar := #{}}}
                , merge(#{foo => #{}}, #{bar => #{}})
                ),
    ?assertMatch( {clashing_keys, [foo]}
                , merge(#{foo => #{}}, #{foo => #{}})
                ).

namespace_test() ->
    ?assertMatch( #{foo := #{bar := #{baz := #{}}}}
                , namespace([foo, bar], #{baz => #{}})
                ).

-endif.

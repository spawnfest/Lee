-module(lee_model).

%% API exports
-export([ desugar/1
        , traverse/3
        , map/2
        , map_with_key/2
        , get/2
        , mk_metatype_index/1
        ]).

%%====================================================================
%% Types
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%% ====================================================================

%% @doc Make an index of MOCs belonging to metatypes. Assumes
%% desugared model
-spec mk_metatype_index(lee:model_fragment()) ->
                           #{lee:metatype() => [map_sets:set(lee:key())]}.
mk_metatype_index(MF) ->
  {_, Idx} = traverse( fun(Key, {Metatypes, _, _}, Acc) ->
                           lists:fold( fun(MT, Acc) ->
                                           #{MT := L0} = Acc,
                                           Acc#{MT => [Key | ]}
                                          , Acc
                                          , Metatypes
                            )
                       end
                     , #{}
                     , MF
                     ),
  Idx.

%% @doc Get a MOC from the model, assumes that the key is present
-spec get(lee:key(), lee:model_fragment()) ->
             lee:moc().
get([Id], MF) ->
    maps:get(Id, MF);
get([Id|Rest], MF) ->
    case maps:get(Id, MF) of
      Map when is_map(Map) ->
        get(Rest, Map);
      {_, _, Children} ->
        get(Rest, Children)
    end.

%% @doc Transform all MOs to fully-qualified form
-spec desugar(lee:model_fragment()) -> lee:model_fragment().
desugar(M0) ->
    {M, _} = traverse(fun desugar_mo/3, undefined, M0),
    M.

-spec map( fun((lee:properties()) -> lee:properties())
         , lee:model_fragment()
         ) -> lee:model_fragment().
map(Fun, M) ->
    map_with_key( fun(_, Attrs) ->
                          Fun(Attrs)
                  end
                , M
                ).

-spec map_with_key( fun((lee:key(), lee:properties()) -> lee:properties())
                  , lee:model_fragment()
                  ) -> lee:model_fragment().
map_with_key(Fun, M) ->
    {Term, _Acc} =
        traverse( fun(Key, MO, _) ->
                          case MO of
                              {Metatype, Attrs} ->
                                  {{Metatype, Fun(Key, Attrs)}, undefined};
                              {Metatype, Attrs, Children} ->
                                  {{Metatype, Fun(Key, Attrs), Children}, undefined}
                          end
                  end
                , undefined
                , M
                ),
    Term.

-spec traverse( fun((lee:key(), lee:moc(), Acc) -> {lee:moc(), Acc})
              , Acc
              , lee:model_fragment()
              ) -> {lee:model_fragment(), Acc}
              when Acc :: term().
traverse(Fun, Acc0, M) ->
    traverse([], Fun, Acc0, M).

traverse(Key0, Fun, AccIn, M) when is_map(M) ->
    maps:fold( fun(K, Val0, {Map0, Acc0}) ->
                       Key = Key0 ++ [K],
                       {Val, Acc} = traverse(Key, Fun, Acc0, Val0),
                       {Map0#{K => Val}, Acc}
               end
             , {#{}, AccIn}
             , M
             );
traverse(Key, Fun, Acc0, MO0) ->
    {MO, Acc1} = Fun(Key, MO0, Acc0),
    case MO of
        {_, _} ->
            {MO, Acc1};
        {Metatype, Attrs, Children0} ->
            {Children, Acc} = traverse(Key, Fun, Acc1, Children0),
            {{Metatype, Attrs, Children}, Acc}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
desugar_mo(_, {MetaTypes, Attrs}, _) ->
    {{MetaTypes, Attrs, #{}}, undefined};
desugar_mo(_, MO = {_, _, _}, _) ->
    {MO, undefined}.

-ifdef(TEST).

-define(mo(Attr, Children), {[t1], Attr, Children}).

-define(mo(Attr), {[t1], Attr}).

-define(model(Attr), #{ foo => ?mo(Attr#{key => [foo]})
                      , bar => ?mo(Attr#{key => [bar]}, #{})
                      , baz => ?mo( Attr#{key => [baz]}
                                  , #{quux => ?mo(Attr#{key => [baz, quux]}, #{})}
                                  )
                      }).

traverse_test() ->
    CheckKey =
        fun(Key, MO, Acc) ->
                case MO of
                    {[t1], #{key := Key}} ->
                        ok;
                    {[t1], #{key := Key}, _} ->
                        ok
                end,
                {MO, Acc + 1}
        end,
    ?assertEqual( {?model(#{}), 4}
                , traverse(CheckKey, 0, ?model(#{}))
                ).

desugar_test() ->
    ?assertEqual( ?model(#{}) #{foo =>
                                    ?mo(#{key => [foo]}, #{})}
                , desugar(?model(#{}))
                ).

get_test() ->
    ?assertEqual( ?mo(#{key => [foo]})
                , lee_model:get([foo], ?model(#{}))
                ),
    ?assertEqual( ?mo(#{key => [baz, quux]}, #{})
                , lee_model:get([baz, quux], ?model(#{}))
                ).

-endif.

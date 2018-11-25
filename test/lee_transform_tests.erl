-module(lee_transform_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("lee/include/lee_types.hrl").

-define(typedef(Type, TypeVars),
        {[typedef]
        , #{ type           => Type
           , type_variables => TypeVars
           }
        , #{}
        }).

-lee_ignore([ignored/0]).
-type ignored() :: string().

-type simple() :: boolean().

-type simple(A) :: A.

-type foo_atom() :: foo.

%% Recursive type is fine too:
-type stupid_list(A) :: {cons, A, stupid_list(A)} | nil.

-lee_verify({url/0, is_url/0}).
-type url() :: string().

type_refl_test() ->
    Model = lee:type_refl([foo, bar], [ simple/0
                                      , simple/1
                                        %% , foo_atom/0
                                        %% , stupid_list/1
                                       ]),
    ?assertEqual( ?typedef(boolean(), [])
                , catch lee_model:get([foo, bar, {simple, 0}], Model)
                ),
    ?assertEqual( ?typedef({var, 1}, [1])
                , lee_model:get([foo, bar, {simple, 1}], Model)
                ),
    ok.

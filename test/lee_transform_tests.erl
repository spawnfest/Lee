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

-type foo_atom() :: foo.

-type simple(A) :: A.

-type simple() :: boolean().

-type strings() :: list(string()).

-type foobar() :: foo | bar | baz.

-type my_tuple() :: {float(), float(), xxx}.

-type list_of_bools() :: [boolean()].

-type non_empty_list_of_bools() :: [boolean(), ...].

-type my_int() :: non_neg_integer().

-type my_byte() :: 0..255.

%% Recursive type is fine too:
-type stupid_list(A) :: {cons, A, stupid_list(A)} | nil.

-lee_verify({url/0, is_url/0}).
-type url() :: string().

type_refl_test() ->
    Model = lee:type_refl([foo, bar], [ simple/0
                                      , simple/1
                                      , foo_atom/0
                                      , strings/0
                                      , foobar/0
                                      , my_tuple/0
                                      , list_of_bools/0
                                      , non_empty_list_of_bools/0
                                      , my_int/0
                                      , my_byte/0
                                      %%, stupid_list/1
                                      ]),
    ?assertEqual( ?typedef(foo, [])
                , catch lee_model:get([foo, bar, {foo_atom, 0}], Model)
                ),
    ?assertEqual( ?typedef({var, 0}, [0])
                , catch lee_model:get([foo, bar, {simple, 1}], Model)
                ),
    ?assertEqual( ?typedef(boolean(), [])
                , catch lee_model:get([foo, bar, {simple, 0}], Model)
                ),
    ?assertEqual( ?typedef(list(string()), [])
                , catch lee_model:get([foo, bar, {strings, 0}], Model)
                ),
    ?assertEqual( ?typedef(union([foo, bar, baz]), [])
                , catch lee_model:get([foo, bar, {foobar, 0}], Model)
                ),
    ?assertEqual( ?typedef(tuple([float(), float(), xxx]), [])
                , catch lee_model:get([foo, bar, {my_tuple, 0}], Model)
                ),
    ?assertEqual( ?typedef(list(boolean()), [])
                , catch lee_model:get([foo, bar, {list_of_bools, 0}], Model)
                ),
    ?assertEqual( ?typedef(nonempty_list(boolean()), [])
                , catch lee_model:get([foo, bar, {non_empty_list_of_bools, 0}], Model)
                ),
    ?assertEqual( ?typedef(non_neg_integer(), [])
                , catch lee_model:get([foo, bar, {my_int, 0}], Model)
                ),
    ?assertEqual( ?typedef(range(0, 255), [])
                , catch lee_model:get([foo, bar, {my_byte, 0}], Model)
                ),
    ok.

-module(lee_transform_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("lee/include/lee_types.hrl").

-lee_ignore([version/0]).
-type version() :: string().

-type checkout() :: tag | branch | commit.

%% Recursive type is fine too:
-type stupid_list(A) :: {A, stupid_list(A)} | nil.

-lee_verify({git_repo/0, is_url/0}).
-type git_repo() :: string().

-type dep_spec() :: {atom(), version()} %% Hex-style
                  | {atom(), {git, git_repo()}, {checkout(), string()}} %% Git
                  .


type_refl_tests() ->
    Gooo = #{},
    Zooo = #{type => foo},
    GeneratedModel = lee_type_refl([foo, bar], [ dep_spec/0
%                                               , stupid_list/1
                                               ]),
    ?assertMatch( #{foo := #{
                      bar := #{}
                     }}
                , GeneratedModel
                ).

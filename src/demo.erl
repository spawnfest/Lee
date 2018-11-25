-module(demo).

-export([main/1]).

-type stupid_list(A) :: {cons, A, stupid_list(A)}
                      | nil
                      .

-type version() :: string().

-type checkout() :: tag | branch | commit.

-type dep_spec() :: {atom(), version()} %% Hex-style
                  | {atom(), {git, string()}, {checkout(), string()}} %% Git
                  .

-type maybe_stupid_list(A) :: list(A)
                            | stupid_list(A)
                            .

-include("lee_types.hrl").

%% Configuration model, it validates CLI parameters and environment
%% variables:
model1() ->
    MyModel = #{ file => {[value, environment_variable, cli_param, valid_file]
                         , #{ mandatory => true
                            , type => string()
                            , oneliner => "Path to the eterm file to verify"
                            , doc => "Path to the eterm file containing a value of\n"
                                     "maybe_stupid_list() type at `list' key"
                            , env => "FILE"
                            , cli_param => "file"
                            , cli_short => $f
                            }}
               , bar => {[value, cli_param]
                        , #{ type => integer()
                           , oneliner => "This value controls baring"
                           , doc => "Blah blah blah"
                           , default => 42
                           , cli_param => "bar"
                           , cli_short => $b
                           }}
               },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  , lee_map_getter:model()
                                  , lee_cli:metamodel()
                                  , lee_env:metamodel()
                                  , MyModel
                                  ]),
    Model.

%% Term model, it validates files:
model2() ->
    TermModel = #{ list => {[value, consult]
                           , #{ type => maybe_stupid_list(atom())
                              , mandatory => true
                              , oneliner => "List, perhaps stupid one"
                              , doc => "Just to check that recursive types work too"
                              , file_key => list
                              }}
                 , deps => {[value, consult]
                           , #{ type => list(dep_spec())
                              , oneliner => "List of dependencies"
                              , doc => "Totally unrelated to rebar3"
                              , file_key => deps
                              , default => []
                              }}
                 },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  , lee_map_getter:model()
                                  , lee_consult:metamodel()
                                  , lee:type_refl([my_types], [ maybe_stupid_list/1
                                                              , dep_spec/0
                                                              ])
                                  , TermModel
                                  ]),
    Model.


main(Args) ->
    CfgModel = model1(),
    TermModel = model2(),
    Config = maps:merge( lee_env:read(CfgModel)
                       , lee_cli:read(CfgModel, Args)
                       ),
    case lee:validate(CfgModel, Config) of
        {ok, _} ->
            {ok, File} = lee:get(CfgModel, Config, [file]),
            {ok, Bar} = lee:get(CfgModel, Config, [bar]),
            io:format("file: ~p~n" "bar: ~p~n", [File, Bar]),
            Terms = lee_consult:read(TermModel, File),
            case lee:validate(TermModel, Terms) of
                {ok, _} ->
                    {ok, List} = lee:get(TermModel, Terms, [list]),
                    {ok, Deps} = lee:get(TermModel, Terms, [deps]),
                    io:format("list: ~p~ndeps: ~p~n", [List, Deps]);
                {error, Errors, _Warnings} ->
                    io:format( "Validation failed:~n~s~n"
                             , [string:join(Errors, "\n")]
                             ),
                    halt(1)
            end;
        {error, Errors, _Warnings} ->
            io:format( "Invalid config:~n~s~n"
                     , [string:join(Errors, "\n")]
                     ),
            halt(1)
    end.

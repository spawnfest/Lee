-module(demo).

-export([main/1]).

-type stupid_list(A) :: {cons, A, stupid_list(A)}
                      | nil
                      .

-type maybe_stupid_list(A) :: list(A)
                            | stupid_list(A)
                            .

-include("lee_types.hrl").

%% Create the configuration data model
model() ->
    MyModel = #{ file => {[value, environment_variable, cli_param]
                         , #{ mandatory => true
                            , type => string()
                            , oneliner => "Path to the eterm file to verify"
                            , doc => "Path to the eterm file containing a value of stupid_list()"
                                     "type at `list' key"
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
               , list => {[value, consult]
                         , #{ type => maybe_stupid_list(atom())
                            , mandatory => true
                            , oneliner => "Term to verify"
                            , doc => "It should be a list of some sort"
                            , file_key => term
                            }}
               },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  , lee_cli:metamodel()
                                  , lee_env:metamodel()
                                  , lee_map_getter:model()
                                  , lee:type_refl([my_types], [ stupid_list/1
                                                              , maybe_stupid_list/1
                                                              ])
                                  , MyModel
                                  ]),
    Model.

main(Args) ->
    Model = model(),
    Config0 = lee_env:read(Model),
    Config1 = lee_cli:read(Model, Args),
    %% Let's suppose CLI arguments should override environment variables:
    Config01 = maps:merge(Config0, Config1),
    %% Get filename from the config received so far:
    case lee:get(Model, Config01, [file]) of
        {ok, Filename} -> ok;
        _ -> Filename = ""
    end,
    Config2 = lee_consult:read(Model, Filename),
    Config = maps:merge(Config01, Config2),
    case lee:validate(Model, Config) of
        {ok, _Warnings} ->
            {ok, File} = lee:get(Model, Config, [file]),
            {ok, Bar} = lee:get(Model, Config, [bar]),
            {ok, List} = lee:get(Model, Config, [list]),
            io:format( "file: ~p~n"
                       "bar: ~p~n"
                       "list: ~p~n"
                     , [File, Bar, List]
                     );
        {error, Errors, _Warnings} ->
            io:format("Invalid config: ~p~n", [Errors]),
            halt(1)
    end.

-module(demo).

-export([main/1]).

-include("lee_types.hrl").

%% Create the configuration data model
model() ->
    MyModel = #{ foo => {[value, environment_variable, cli_param]
                        , #{ mandatory => true
                           , type => string()
                           , oneliner => "This value controls fooing"
                           , doc => "Blah blah blah"
                           , env => "FOO"
                           , cli_param => "foo"
                           , cli_short => "f"
                           }}
               , bar => {[value, cli_param]
                        , #{ type => integer()
                           , oneliner => "This value controls baring"
                           , doc => "Blah blah blah"
                           , default => 42
                           , cli_param => "bar"
                           , cli_short => "b"
                           }}
               },
    {ok, Model} = lee_model:merge([ lee:base_model()
                                  , lee:base_metamodel()
                                  %%, lee_cli:metamodel()
                                  , lee_env:metamodel()
                                  , lee_map_getter:model()
                                  , MyModel
                                  ]),
    Model.

main(Args) ->
    Model = model(),
    Config = lee_env:read(Model),
    %%Config1 = lee_cli:read(Model, CliAttrs),
    %% Let's suppose CLI arguments should override environment variables:
    %Config = maps:merge(Config0, Config1),
    case lee:validate(Model, Config) of
        {ok, _Warnings} ->
            {ok, Foo} = lee:get(Model, Config, [foo]),
            {ok, Bar} = lee:get(Model, Config, [bar]),
            io:format( "foo: ~p~n"
                       "bar: ~p~n"
                     , [Foo, Bar]
                     );
        {error, Errors, _Warnings} ->
            io:format("Invalid config: ~p~n", [Errors]),
            halt(1)
    end.

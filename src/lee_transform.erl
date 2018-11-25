-module(lee_transform).

-export([parse_transform/2]).

%% TODO: Source code locations are... approximate
%% TODO: Error handling is inexistent
%% TODO: Parse `-export_type' attribute and export lee types automatically

-record(s, %% TODO: field types
        { typedefs
        , custom_verif
        , types_to_reflect
        , types_to_export
        }).

-define(INT(Line, Val),
        {integer, Line, Val}).

-define(INT(Val),
        ?INT(_, Val)).

-define(ATOM(Line, Atom),
        {atom, Line, Atom}).

-define(ATOM(Atom),
        ?ATOM(_, Atom)).

-define(LCALL(Line, Name, Args),
        {call, Line, ?ATOM(Name), Args}).

-define(RCALL(Line, Module, Function, Args),
        {call, Line
        , {remote, _, ?ATOM(Module), ?ATOM(Function)}
        , Args
        }).

-define(MK_RCALL(Line, Module, Function, Args),
        {call, Line
        , {remote, Line, ?ATOM(Line, Module), ?ATOM(Line, Function)}
        , Args
        }).

-define(MK_TYPEDEF(Line, Name, Arity),
        {tuple, Line
        , [ {cons, Line, ?ATOM(Line, typedef), {nil, Line}}
          , {map, Line, [ {map_field_assoc, Line
                          , ?ATOM(Line, type)
                          , {atom,25,foo}
                          }
                        ]}
          , {map, Line, []}
          ]
        }).

-define(LTYPE_REF(Name, Arity),
        {op, _, '/', ?ATOM(Name), {integer, _, Arity}}).

parse_transform(Forms0, _Options) ->
    Ignored = ignored(Forms0),
    CustomVerif = custom_verify(Forms0),
    Typedefs0 = local_typedefs(Forms0),
    Typedefs = maps:without(Ignored, Typedefs0),
    io:format( "~p~nIgnored: ~p~nCustom: ~p~nTypes ~p~n"
             , [Forms0, Ignored, CustomVerif, Typedefs]
             ),
    State0 = #s{ typedefs = Typedefs
               , custom_verif = CustomVerif
               , types_to_reflect = []
               , types_to_export = []
               },
    {Forms1, State} = forms(State0, Forms0),
    Forms1.

forms(State0, ?RCALL(Line, lee, type_refl, [Namespace, Types0])) ->
    Types1 =
        literal_list( fun(?LTYPE_REF(Name, Arity)) ->
                              {Name, Arity}
                      end
                    , Types0
                    ),
    Types =
        mk_literal_list( Line
                       , fun({Name, Arity}) ->
                                 Key = {tuple, Line
                                       , [?ATOM(Line, Name), ?INT(Line, Arity)]
                                       },
                                 Val = ?ATOM(Line, undefined),
                                 {tuple, Line, [Key, Val]}
                         end
                       , Types1
                       ),
    erlang:display({types, Types}),
    AST = ?MK_RCALL(Line, lee, namespace, [ Namespace
                                          , ?MK_RCALL(Line, maps, from_list, [Types])
                                          ]),
    State = State0,
    {AST, State};
forms(State0, L) when is_list(L) ->
    {ASTL, State} = lists:foldl( fun(I, {L0, State0}) ->
                                         {V, State} = forms(State0, I),
                                         {[V|L0], State}
                                 end
                               , {[], State0}
                               , L
                               ),
    {lists:reverse(ASTL), State};
forms(State0, T) when is_tuple(T) ->
    L = tuple_to_list(T),
    {AST, State} = forms(State0, L),
    {list_to_tuple(AST), State};
forms(State, AST) ->
    {AST, State}.

ignored(Forms) ->
    DeepDefs = [Defs || {attribute, _, lee_ignore, Defs} <- Forms],
    lists:usort(lists:append(DeepDefs)).

custom_verify(Forms) ->
    Defs = [Def || {attribute, _, lee_verify, Def} <- Forms],
    maps:from_list(Defs).

local_typedefs(Forms) ->
    maps:from_list([{{Name, length(Params)}, {AST, Params}}
                    || { attribute
                       , _Line
                       , type
                       , {Name, AST, Params}
                       } <- Forms
                   ]).

literal_list(_, {nil, _}) ->
    [];
literal_list(Fun, {cons, _, Val, Tail}) ->
    [Fun(Val) | literal_list(Fun, Tail)].

mk_literal_list(Line, _, []) ->
    {nil, Line};
mk_literal_list(Line, Fun, [Val|Tail]) ->
    {cons, Line, Fun(Val), mk_literal_list(Line, Fun, Tail)}.

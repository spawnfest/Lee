-module(lee_transform).

-export([parse_transform/2]).

%% TODO: Source code locations are... approximate
%% TODO: Error handling is inexistent
%% TODO: Parse `-export_type' attribute and export lee types automatically

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

parse_transform(Forms, _Options) ->
    Ignored = ignored(Forms),
    CustomVerif = custom_verify(Forms),
    Typedefs0 = local_typedefs(Forms),
    Typedefs = maps:without(Ignored, Typedefs0),
    io:format( "~p~nIgnored: ~p~nCustom: ~p~nTypes ~p~n"
             , [Forms, Ignored, CustomVerif, Typedefs]
             ),
    forms([], Forms).

forms(LocalTypes, Foo = ?LCALL(Line, lee_type_refl, [Namespace, Types0])) ->
    try
        Types1 =
            literal_list( fun(?LTYPE_REF(Name, Arity)) ->
                                  {Name, Arity}
                          end
                        , Types0
                        ),
        Types =
            mk_literal_list( Line
                           , fun({Name, Arity}) ->
                                     {tuple, Line
                                     , [?ATOM(Line, Name), ?INT(Line, Arity)]
                                     }
                             end
                           , Types1
                           ),
        erlang:display({types, Types}),
        ?MK_RCALL(Line, lee, namespace, [Namespace, Types])
    catch
        {trans_error, Line, Message, Args} ->
            error(Line) %% TODO do something smart instead of crashing
    end;
forms(LocalTypes, L) when is_list(L) ->
    [forms(LocalTypes, Term) || Term <- L];
forms(LocalTypes, T) when is_tuple(T) ->
    L = tuple_to_list(T),
    list_to_tuple(forms(LocalTypes, L));
forms(_, T) ->
    T.

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

-module(scxml).

-export([demo/0, load/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/scxml.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

demo() ->
    File = filename:join(code:priv_dir(scxml), "demo.scxml"),
    {ok, C} = load(File),
    error_logger:info_msg("~p", [C]),
    {ok, _Pid} = scxml_proc:start_link(C).

load(File) ->
    case xmerl_scan:file(File) of
        {#xmlElement{name = scxml, attributes = A, content = C}, _} ->
            Attrs = attr(A),
            Id = proplists:get_value(name, Attrs),
            Initial = proplists:get_value(initial, Attrs),
            State = reduce(C, #state{id = Id, initial = Initial}),
            {ok, State};
        Other -> Other
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reduce([], Acc) -> Acc;
reduce([#xmlElement{name = state, attributes = A, content = C} | T], #state{state = L} = Acc) ->
    Attrs = attr(A),
    Id = proplists:get_value(id, Attrs),
    Initial = proplists:get_value(initial, Attrs),
    State = reduce(C, #state{id = Id, initial = Initial}),
    reduce(T, Acc#state{state = [State | L]});
reduce([#xmlElement{name = final, attributes = A, content = C} | T], #state{state = L} = Acc) ->
    Attrs = attr(A),
    State = reduce(C, #state{id = proplists:get_value(id, Attrs), final = true}),
    reduce(T, Acc#state{state = [State | L]});
reduce([#xmlElement{name = transition, attributes = A, content = C} | T], #state{transition = L} = Acc) ->
    Attrs = attr(A),
    Event = proplists:get_value(event, Attrs),
    Cond = proplists:get_value('cond', Attrs),
    Target = proplists:get_value(target, Attrs),
    Type = proplists:get_value(type, Attrs),
    Execute = reduce(C, []),
    E = #transition{event = Event, 'cond' = Cond, target = Target, type = Type,
                    execute = Execute},
    reduce(T, Acc#state{transition = [E | L]});
reduce([#xmlElement{name = onentry, content = C} | T], #state{} = Acc) ->
    reduce(T, Acc#state{onentry = lists:reverse(reduce(C, []))});
reduce([#xmlElement{name = onexit, content = C} | T], #state{} = Acc) ->
    reduce(T, Acc#state{onexit = lists:reverse(reduce(C, []))});
reduce([#xmlElement{name = send, attributes = A} | T], Acc) ->
    Attrs = attr(A),
    Id = proplists:get_value(id, Attrs),
    Event = proplists:get_value(event, Attrs),
    Target = proplists:get_value(target, Attrs),
    Delay = erlang:list_to_integer(proplists:get_value(delay, Attrs)),
    E = #send{id = Id, event = Event, target = Target, delay = Delay},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = log, attributes = A} | T], Acc) ->
    Attrs = attr(A),
    Label = proplists:get_value(label, Attrs),
    E = #log{label = Label},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = raise, attributes = A} | T], Acc) ->
    Attrs = attr(A),
    E = #raise{event = proplists:get_value(event, Attrs)},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = cancel, attributes = A} | T], Acc) ->
    Attrs = attr(A),
    E = #cancel{sendid = proplists:get_value(sendid, Attrs)},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = script, content = C} | T], Acc) ->
    [S] = [V || #xmlText{value = V} <- C],
    E = #script{content = parse(S)},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = assign, attributes = A} | T], Acc) ->
    Attrs = attr(A),
    Loc = proplists:get_value(location, Attrs),
    Expr = proplists:get_value(expr, Attrs),
    E = #assign{location = Loc, expr = parse(Expr)},
    reduce(T, [E | Acc]);
reduce([#xmlElement{name = 'if', attributes = A, content = C} | T], Acc) ->
    Attrs = attr(A),
    Cond = proplists:get_value('cond', Attrs),
    L = [E || #xmlElement{} = E <- C],
    L1 = lists:takewhile(fun(#xmlElement{name = N}) -> N =/= 'else' end, L),
    R = lists:dropwhile(fun(#xmlElement{name = N}) -> N =/= 'else' end, L),
    [#xmlElement{name = 'else'} | L2] = R,
    E = #'if'{'cond' = parse(Cond),
              then = lists:reverse(reduce(L1, [])),
              'else' = lists:reverse(reduce(L2, []))},
    reduce(T, [E | Acc]);
reduce([_ | T], Acc) -> reduce(T, Acc).

attr(E) -> [{N, V} || #xmlAttribute{name = N, value = V} <- E].

parse(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
    ExprList.

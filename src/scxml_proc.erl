-module(scxml_proc).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record('State', {chart, current}).

-include("../include/scxml.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(Chart) -> gen_server:start_link(?MODULE, Chart, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#state{id = Id, initial = Initial} = Chart) ->
    error_logger:info_msg("Start proc: ~s", [Id]),
    erlang:send_after(0, self(), {enter}),
    {ok, #'State'{chart = Chart, current = Initial}}.

handle_info({final}, State = #'State'{}) ->
    {stop, normal, State};
handle_info({enter}, State = #'State'{chart = Chart, current = StateId}) ->
    Path = find_path(StateId, Chart),
    enter(Path),
    {noreply, State};
handle_info({timer, Id, E}, State = #'State'{chart = C, current = StateId}) ->
    error_logger:info_msg("[~s] timer event: ~s", [StateId, E]),
    erase(Id),
    Path = find_path(StateId, C),
    case find(E, #transition.event, (lists:last(Path))#state.transition) of
        #transition{target = undefined, execute = L} ->
            execute(L),
            {noreply, State};
        #transition{target = Target, execute = L} = T ->
            move(Path, T, C),
            {noreply, State#'State'{current = Target}}
    end;
handle_info({event, E}, State = #'State'{chart = C, current = StateId}) ->
    error_logger:info_msg("[~s] event: ~s", [StateId, E]),
    Path = find_path(StateId, C),
    #transition{target = Target} = T = find(E, #transition.event, (lists:last(Path))#state.transition),
    move(Path, T, C),
    {noreply, State#'State'{current = Target}}.

terminate(_Reason, _State = #'State'{chart = #state{id = Id}}) ->
    error_logger:info_msg("terminated proc: ~s", [Id]),
    ok.

code_change(_OldVsn, State = #'State'{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

execute(L) when is_list(L) ->
    [begin error_logger:info_msg("execute: ~p", [E]), execute(E) end || E <- L];
execute(#raise{event = E}) -> self() ! {event, E};
execute(#send{id = Id, event = E, delay = D}) ->
    case get(Id) of
        undefined ->
            TRef = erlang:send_after(D, self(), {timer, Id, E}),
            put(Id, TRef);
        _ -> error_logger:error_msg("Timer exists for id ~p", [Id])
    end;
execute(#assign{}) -> ok;
execute(#log{label = L}) -> error_logger:info_msg("~s", [L]);
execute(#cancel{sendid = Id}) ->
    case erase(Id) of
        undefined -> error_logger:error_msg("Timer not found for id ~p", [Id]);
        TRef -> erlang:cancel_timer(TRef)
    end.

move(From, #transition{target = Target, execute = L}, Chart) ->
    To = find_path(Target, Chart),
    P = find_prefix(From, To),
    leave(lists:reverse(lists:subtract(From, P))),
    execute(L),
    enter(lists:subtract(To, P)),
    S = lists:last(To),
    case S#state.final of
        true ->
            leave([S]),
            erlang:send_after(0, self(), {final});
        _ -> ok
    end.

find(Id, Pos, L) -> [T] = [T || T <- L, erlang:element(Pos, T) =:= Id], T.

enter([]) -> ok;
enter([#state{id = Id, onentry = L} | T]) ->
    error_logger:info_msg("enter ~s", [Id]),
    execute(L),
    enter(T).

leave([]) -> ok;
leave([#state{id = Id, onexit = L} | T]) ->
    error_logger:info_msg("exit ~s", [Id]),
    execute(L),
    leave(T).

find_path(Id, #state{state = L}) -> find_path(Id, L, []).

find_path(_, [], Acc) -> Acc;
find_path(Id, [#state{id = Id} = S | _], Acc) -> [S | Acc];
find_path(Id, [#state{state = L} = S | T], Acc) ->
    case find_path(Id, L, []) of
        [] -> find_path(Id, T, Acc);
        P -> [S | P]
    end.

find_prefix(L1, L2) -> find_prefix(L1, L2, []).

find_prefix([H | T1], [H | T2], Acc) -> find_prefix(T1, T2, [H | Acc]);
find_prefix(_, _, Acc) -> lists:reverse(Acc).
-module(counter).
-behaviour(gen_server).

-define(MASTER_NODE, 'counter@jurmacbookpro.local').

-export([
         start_link/0,
         get_value/1,
         increment/1,
         decrement/1 ]).
-export([
         init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3
        ]).

start_link() when node() =:= ?MASTER_NODE ->
     start_link({0,0});

start_link() ->
    case net_kernel:connect_node(?MASTER_NODE) of
      true ->
        start_link({0,0});
      _    ->
        halt(1)
     end.

start_link(InitialValue) ->
    Result = {ok, Pid} = gen_server:start_link( ?MODULE, InitialValue, []),
    pg2:create(pn_counter),
    case pg2:get_closest_pid(pn_ounter) of
      {error, _}  -> ok;
      []          -> ok;
      RemoteNode  -> RemoteNode ! {pn_counter_request, Pid}
    end,
    pg2:join(pn_counter, Pid),
    Result.


get_value(Pid) ->
  gen_server:call(Pid, get_value).

increment(Pid) ->
  gen_server:cast(Pid, increment).

decrement(Pid) ->
  gen_server:cast(Pid, decrement).


%% GenServer

init(CurrentValue) ->
    { ok, CurrentValue }.

handle_call(get_value, _From, {PCounter, NCounter}) ->
    CalculatedValue = PCounter - NCounter,
    { reply, CalculatedValue, {PCounter, NCounter} }.

handle_cast(increment, {PCounter, NCounter}) ->
    NewValue = {PCounter + 1, NCounter},
    downstream(NewValue),
    { noreply, NewValue };

handle_cast(decrement, {PCounter, NCounter}) ->
    NewValue = {PCounter, NCounter + 1},
    downstream(NewValue),
    { noreply, NewValue }.

handle_info({pn_counter_value, {RemP, RemN}}, {PCounter, NCounter}) ->
    {noreply, { max(PCounter, RemP), max(NCounter, RemN) }};

handle_info({pn_counter_request, From}, CurrentValue) ->
    From ! {pn_counter_value, CurrentValue},
    {noreply, CurrentValue}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Helpers
downstream(Value) ->
    net_kernel:connect_node(?MASTER_NODE),
    GrMembers = pg2:get_members(pn_counter),
    case GrMembers of
      {error, _} -> ok;
      []         -> ok;
      Pids       -> downstream(Pids, Value)
    end.

downstream([], _Value) -> ok;

downstream([Pid | Rest], Value) when Pid =:= self() ->
    downstream(Rest, Value);

downstream([Pid | Rest], Value) ->
    Pid ! {pn_counter_value, Value},
    downstream(Rest, Value).

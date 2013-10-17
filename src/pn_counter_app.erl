-module(pn_counter_app).
-author('Yury Batenko <jurbat@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, CounterPid} = pn_counter_sup:start_link(),
    Routes    = routes(CounterPid),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts).

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

routes(CounterPid) ->
    [ {'_', [
               {"/counter",           counter_handler,   [CounterPid]},
               {"/counter/increment", increment_handler, [CounterPid]},
               {"/counter/decrement", decrement_handler, [CounterPid]}
            ]}
    ].

port() ->
    case os:getenv("PNCOUNTER_PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
     end.

-module(increment_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, [PidCounter]) ->
    {ok, Req, PidCounter}.

handle(Req, PidCounter) ->
    case cowboy_req:get(method, Req) of
        <<"POST">> ->
            counter:increment(PidCounter),
            {ok, Req} = cowboy_req:reply(201, [], <<>>, Req);
        _ ->
            {ok, Req} = cowboy_req:reply(405, Req)
    end,
    {ok, Req, PidCounter}.

terminate(_Reason, _Req, _PidCounter) ->
    ok.

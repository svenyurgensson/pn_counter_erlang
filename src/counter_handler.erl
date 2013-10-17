-module(counter_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, [PidCounter]) ->
    {ok, Req, PidCounter}.

handle(Req, PidCounter) ->
    case cowboy_req:get(method, Req) of
        <<"GET">> ->
            Value = counter:get_value(PidCounter),
            {ok, Req} = cowboy_req:reply(200, [], integer_to_binary(Value), Req);
        _ ->
            {ok, Req} = cowboy_req:reply(405, Req)
    end,
    {ok, Req, PidCounter}.

terminate(_Reason, _Req, _PidCounter) ->
    ok.

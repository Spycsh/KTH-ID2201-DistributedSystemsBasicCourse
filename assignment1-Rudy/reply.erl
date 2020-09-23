-module(reply).
-export([start/1, stop/0]).

start(Port)->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen}->
            % 
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error}->
            io:format("rudy: error1: ~w~n", [Error])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            io:format("rudy: error2: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error3: ~w~n", [Error])
    end,
    gen_tcp:close(Client).


reply({{get, _, _}, _, Body}) ->
    % timer:sleep(20),
    http:ok(Body).

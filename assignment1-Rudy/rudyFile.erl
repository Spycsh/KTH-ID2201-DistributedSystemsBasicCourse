-module(rudyFile).
-export([start/1, stop/0]).

start(Port)->
    register(rudyFile, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudyFile), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen}->
            % 
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error}->
            io:format("rudyFile: error1: ~w~n", [Error])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            io:format("rudyFile: error2: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudyFile: error3: ~w~n", [Error])
    end,
    gen_tcp:close(Client).


reply({{get, URI, _}, _, _}) ->
    FileSize = filelib:file_size(URI),
    {ok, Body} = file:read_file(URI),
    {Name, Type} = type(URI),
    % timer:sleep(20),
    % http:ok(Body).
    HeaderLines = "Content-Length: " ++ integer_to_list(FileSize) ++ "\r\n" ++ 
"Content-Type: " ++ Type ++"\r\n",
    ok(HeaderLines, Body).


ok(HeaderLines, Body) ->
    % io:format("HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body),
    "HTTP/1.1 200 OK\r\n" ++ HeaderLines ++ "\r\n" ++ Body.

type([46|R0]) -> 
    {[], R0};
type([C|R0]) ->
    {Rest,R1} = type(R0),
    {[C|Rest],R1}.



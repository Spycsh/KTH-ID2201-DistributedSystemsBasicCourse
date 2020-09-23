-module(alogger).

-export([start/1, stop/1]).

% give a list of nodes that will send it messages
% here just ignore the nodes
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).


stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Clock, []).

loop(Clock, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
            MessageQueue = lists:keysort(3,MessageList = HoldBackQueue ++ [{log, From,Time, Msg}]),
            List = time:update(From, Time, Clock),
            case time:safe(Time, List) of
                true ->
                    TempList = logInfo(Time, MessageQueue),
                    loop(List, TempList);
                false ->
                    loop(List, MessageList)
            end;
            % log(From, Time, Msg),
            % loop();
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w~w~p~n", [Time, From, Msg]).


logInfo(_, [])->
    [];
logInfo(Time, [{log, From, MsgTime, Msg}|T]) ->
    if
        MsgTime =< Time -> log(From, MsgTime, Msg),
                            logInfo(Time, T);
        true -> [{log, From, MsgTime, Msg}|T]
    end.
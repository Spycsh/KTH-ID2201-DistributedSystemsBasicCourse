-module(vector_logger).

-export([start/1, stop/1, insertQueue/3]).

% give a list of nodes that will send it messages
% here just ignore the nodes
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).


stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = vector_time:clock(Nodes, vector_time:zero(length(Nodes))),
    loop(Clock, []).

insertQueue([], E, Flag)->
    if Flag -> [];
    true -> [E] end;
insertQueue([H|T], E, Flag) ->
    case E of {_,_,InsertV,_} ->
        case H of {_, _, V, _} -> 
            Res = vector_time:leq(InsertV, V),
            if Res ->
                [E,H|insertQueue(T, E, true)];
            true -> [H|insertQueue(T, E, false)]
            end
        end
    end.


% HoldBackQueue      
% [{log, john, [0,2,1,2], 33},...]
loop(Clock, HoldBackQueue) ->
    receive
        {log, From, Time, Msg} ->
            MessageList = insertQueue(HoldBackQueue,{log, From,Time, Msg}, false),

            List = vector_time:update(From, Time, Clock),
            case vector_time:safe(Time, List) of
                true ->
                    TempList = logInfo(Time, MessageList),
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
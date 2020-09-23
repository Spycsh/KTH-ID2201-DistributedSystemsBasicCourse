-module(vector_worker).
-export([start/7, stop/1, peers/2]).

% should know how many processes there are
start(Index, Name, Logger, Seed, Sleep, Jitter, N) ->
    spawn_link(fun()->init(Index, Name, Logger, Seed, Sleep, Jitter, N) end).

stop(Worker) ->
    Worker ! stop.

init(Index, Name, Log, Seed, Sleep, Jitter, N) ->
    random:seed(Seed, Seed, Seed),
    % MsgClock = 0,
    ZeroVector = vector_time:zero(N),
    receive
        {peers, Peers} ->
            loop(Index, Name, Log, Peers, Sleep, Jitter, ZeroVector);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Index, Name, Log, Peers, Sleep, Jitter, MsgClock) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, TimeVector, Msg} ->
            TempVector = vector_time:merge(TimeVector, MsgClock),
            NewVector = vector_time:inc(Index, TempVector, 1),

            Log ! {log, Name, NewVector, {received, Msg}},
            loop(Index, Name, Log, Peers, Sleep, Jitter, NewVector);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = vector_time:inc(Index, MsgClock, 1),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Index, Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

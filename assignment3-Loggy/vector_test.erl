-module(vector_test).
-export([run/2]).

run(Sleep, Jitter) ->
    Log = vector_logger:start([john, paul, ringo, george]),
    A = vector_worker:start(1, john, Log, 13, Sleep, Jitter, 4),
    B = vector_worker:start(2, paul, Log, 23, Sleep, Jitter, 4),
    C = vector_worker:start(3, ringo, Log, 36, Sleep, Jitter, 4),
    D = vector_worker:start(4, george, Log, 49, Sleep, Jitter, 4),
    vector_worker:peers(A, [B,C,D]),
    vector_worker:peers(B, [A,C,D]),
    vector_worker:peers(C, [A,B,D]),
    vector_worker:peers(D, [A,B,C]),
    timer:sleep(5000),
    vector_logger:stop(Log),
    vector_worker:stop(A),
    vector_worker:stop(B),
    vector_worker:stop(C),
    vector_worker:stop(D).
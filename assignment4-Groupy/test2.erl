-module(test2).
-export([run/0]).

run() ->
    Worker = worker:start(1, gms2, 1, 1000),
    timer:sleep(500),
    worker:start(2, gms2, 2, Worker, 2000),
    timer:sleep(500),
    worker:start(3, gms2, 3, Worker, 3000),
    timer:sleep(500),
    worker:start(4, gms2, 4, Worker, 4000).
    
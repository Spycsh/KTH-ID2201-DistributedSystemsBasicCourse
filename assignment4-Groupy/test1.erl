-module(test1).
-export([run/0]).

run() ->
    Worker = worker:start(1, gms1, 1, 1000),
    timer:sleep(500),
    worker:start(2, gms1, 2, Worker, 2000),
    timer:sleep(500),
    worker:start(3, gms1, 3, Worker, 3000),
    timer:sleep(500),
    worker:start(4, gms1, 4, Worker, 4000).
    
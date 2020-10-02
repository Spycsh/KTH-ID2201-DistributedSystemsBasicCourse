-module(test3).
-export([run/0]).

run() ->
    Worker = worker:start(1, gms3, 1, 1000),
    timer:sleep(500),
    worker:start(2, gms3, 2, Worker, 2000),
    timer:sleep(500),
    worker:start(3, gms3, 3, Worker, 3000),
    timer:sleep(500),
    worker:start(4, gms3, 4, Worker, 4000).
    % timer:sleep(5000),
    % who is the leader now?
    % worker:start(4, gms3, 4, Worker, 4000).
    % max 4 processes
    % time:sleep(500),
    % worker:start(5,gms3, 5, Worker, 5000).
    
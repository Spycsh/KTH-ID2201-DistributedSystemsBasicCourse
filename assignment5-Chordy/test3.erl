-module(test3).
-export([testNode3/0]).

testNode3() ->
  % Start first node
  FirstNode = test:start(node3),
  Node2 = test:start(node3, FirstNode),
  Node3 = test:start(node3, FirstNode),
  Node4 = test:start(node3, FirstNode),
  Node5 = test:start(node3, FirstNode),
  Node6 = test:start(node3, FirstNode),
  Node7 = test:start(node3, FirstNode),
  Node8 = test:start(node3, FirstNode),
  % Sleep to let the ring stabilize
  timer:sleep(10000),
  % Send a probe around the ring
  FirstNode ! probe,
  timer:sleep(5000),
  % Kill nodes
  io:format("Stopping node 1 - PID: ~p~n", [FirstNode]),
  FirstNode ! stop,
  timer:sleep(2000),  % Need timer here so we don't kill two in a row, has to recover
  io:format("Stopping node 3 - PID: ~p~n", [Node3]),
  Node3 ! stop,
  timer:sleep(2000),  % Need timer here so we don't kill two in a row, has to recover
  io:format("Stopping node 7 - PID: ~p~n", [Node7]),
  Node7 ! stop,
  timer:sleep(10000),
  % Send another probe around the ring
  Node8 ! probe.

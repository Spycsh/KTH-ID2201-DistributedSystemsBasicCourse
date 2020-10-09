% Performance test
-module(test0).
-compile(export_all).

% 1 node in ring
performanceTest1() ->
    FirstNode = test:start(node2),
    timer:sleep(1000),
    performanceTest1(FirstNode).
performanceTest1(NodeToContact) ->
    Keys = test:keys(4000),
    test:add(Keys, NodeToContact),
    test:check(Keys, NodeToContact).

% 4 node in ring
% performanceTest2() ->
%     FirstNode = test:start(node2),
%     timer:sleep(1000),
%     % test:start(node2, 3, FirstNode),
%     performanceTest2(FirstNode, 3).

% performanceTest2(_, 0) ->
%     ok;
% performanceTest2(NodeToContact, N) ->
%     % this sleep time is important
%     timer:sleep(4000),

%     Keys = test:keys(1000),
%     test:add(Keys, NodeToContact),
%     T = test:check(Keys, NodeToContact),
%     timer:sleep(4000),
%     Node = test:start(node2, NodeToContact),
%     io:format("~w",[Node]),
%     performanceTest2(Node, N-1).

% N machine each handle 1000 nodes
performanceTest2(0, T1) ->
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("~w~n",[Done]).
performanceTest2(N) ->
    T1 = now(),
    FirstNode = test:start(node2),

    spawn(fun()-> handleOneNode(FirstNode, self()) end),
    receive
        ok -> performanceTest2(N-1, T1),
        performanceTest2
    end.

handleOneNode(Node, Ref) ->
    io:format("1"),
    Keys = test:keys(1000),
    timer:sleep(4000),
    test:add(Keys, Node),
    test:check(Keys, Node),
    Ref ! ok.

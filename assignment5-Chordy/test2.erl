-module(test2).
-export([start/2, stop/1, status/1, add/2, lookup/2]).

% This is the test file for the node2

% Nodes = test2:start(node2, 1000)
% add 1000 elements to the ring
start(Module, Number) ->
    % register the first element in the ring
    Pid = apply(Module, start, [0]),
    Name = 'node0',
    register(Name, Pid),
    timer:sleep(200),
    init(Module, Number, 1, [Pid]).



init(Module, Number, Counter, Spawned) ->
    case Number of
        Counter ->
            Spawned;
        _ ->
            [H|_] = Spawned,    % H: the initial successor
            Pid = apply(Module, start, [Counter, H]),

            Name = "node" ++ integer_to_list(Counter),
            register(list_to_atom(Name), Pid),
            NSpawned = lists:append(Spawned, [Pid]),
            init(Module, Number, Counter + 1, NSpawned)
    end.

% test2:add(node2, 1000).
% add key-value to the ring, a suitable node would take care of the key
add(Node, Number) ->
    Now = erlang:now(),
    [After | Elements] = add_i(Node, Number, []),
    Diff = timer:now_diff(After, Now),
    io:format("Total time to add ~w data in microseconds: ~w~n", [Number, Diff]),
    Elements.
    
add_i(Node, Number, Elements) ->
    case Number of
        0 ->
            [erlang:now() | Elements];
        _ ->
            Rnd = key:generate(),
            Ref = make_ref(),
            Node ! {add, Rnd, key:generate(), Ref, self()},
            receive
                {Ref, ok} ->
                    add_i(Node, Number - 1, [Rnd | Elements])
            end
    end.

% test2:lookup(node2, 996)
lookup(Node, Key) -> 
    Ref = make_ref(),
    Node ! {lookup, Key, Ref,self()},
    receive
        {Ref, {Key, Value}} ->
            io:format("Key: ~w~nValue:~w~n",[Key, Value])
    end.

status(Nodes) ->
    lists:foreach(fun(Node)->Node!state, timer:sleep(10) end, Nodes).

stop(Nodes) ->
    lists:foreach(fun(Node)->Node!stop end, Nodes).


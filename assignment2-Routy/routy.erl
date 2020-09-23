-module(routy).
-export([start/2, stop/1,status/1]).


start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

status(Router) ->
    Router ! {status, self()},
    receive
	{status, {Name, N, Msg, Intf, Table, Map}}->
	    io:format("Status -------------~n"),
	    io:format(" name: ~w~n", [Name]),
	    io:format("    n: ~w~n", [N]),
	    io:format(" msgs: ~w~n", [Msg]),
            io:format(" intf: ~w~n", [Intf]),
            io:format("table: ~w~n", [Table]),
            io:format("  map: ~w~n", [Map]), 
	    ok
    after 4000 ->
	    io:format("No reply -------------~n"),
	    ok
    end.


init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

% a symbolic name such as london
% a counter
% a history of received messages
% a set of interfaces
% a routing table
% a map of the network
router(Name, N, Hist, Intf, Table, Map) ->
    receive
        % if the name is matched, then no further hops needed
        {route, Name, From, Message} ->
            io:format("~w: received message (~s) from ~w~n", [Name, Message, From]),
            router(Name, N, Hist, Intf, Table, Map);
        
        % if the name is not matched, then forward to another hop
        {route, To, From, Message} ->
            io:format("~w: routing message (~s) from ~w to ~w~n", [Name, Message, From, To]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case interfaces:lookup(Gw, Intf) of
                        notfound ->
                            io:format("~w: interface for gw ~w not found ~n", [Name, Gw]);
                        {ok, Pid} ->
                            io:format("~w: forward to ~w~n", [Name, Gw]),
                            Pid ! {route, To, From, Message}
                    end;
                notfound ->
                    io:format("~w: routing entry for ~w not found ~n", [Name, To])
            end,
            % The router process will continuously listen
            router(Name, N, Hist, Intf, Table, Map);
        
        % add a message so that a local user can initiate the routing of a
        % message without knowing the name of the local router.
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        
        % r1 ! {add, goteborg, {r3, 'sweden@192.168.1.3'}
        % stockholm(r1) => goteborg(r3)
        % a monitor will send a ’DOWN’ message to the process 
        % and we can then remove links to the node
        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Intf1 = interfaces:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);

        {remove, Node} ->
            case interfaces:ref(Node, Intf) of
                {ok, Ref} -> 
                    erlang:demonitor(Ref),
                    Intf1 = interfaces:remove(Node, Intf),
                    router(Name, N, Hist, Intf1, Table, Map);
                notfound ->
                    io:format("Remove unknown node!")
            end,
            router(Name, N, Hist, Intf, Table, Map);


        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = interfaces:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = interfaces:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        
        % When receiving a links-state message a
        % router must check if this is an old or new message
        {links, Node, X,Links} ->
            case hist:update(Node, X, Hist) of
                {new, Hist1} ->
                    interfaces:broadcast({links, Node, X, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        
        % The status, From message can be used to do a pretty-print of the
        % state. Add a function that sends a status message to a process, receives
        % the reply and displays the information.
        {status, From} ->
            io:format("STATUS info:"),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        
        update -> 
            Table1 = dijkstra:table(interfaces:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        
        % order our router to broadcast a link-state message
        broadcast ->
            Message = {links, Name, N, interfaces:list(Intf)},
            interfaces:broadcast(Message, Intf),
            % N+1 records the number of times of broadcast-ordering
            router(Name, N+1, Hist, Intf, Table, Map);

        stop ->
            ok
    end.
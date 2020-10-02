-module(gms1).
-export([start/1, start/2]).

% give it an empty list of peers and let it know that
% its master is the only node in the group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).


% starting a node that should join an existing group
% return a process
start(Id, Grp) ->
    Self = self(),  % the worker
    {ok, spawn_link(fun()->init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self}, 
    % all slaves have the same list of peers and they elect the first node in the list
    % as the leader.
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.

leader(Id, Master, Slaves, Group) ->
    receive
        % a message either from its own master or from a peer
        % node. A message fmsg, Msgg is multicasted to all peers and a message
        % Msg is sent to the application layer
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        % a message, from a peer or the master, that is
        % a request from a node to join the group. The message contains both
        % the process identifier of the application layer, Wrk, and the process
        % identifier of its group process.
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        % a request from its master to multicast a message, the
        % message is forwarded to the leader
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        % a request from the master to allow a new node
        % to join the group, the message is forwarded to the leader
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        % a multicasted message from the leader. A message Msg
        % is sent to the master
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        % a multicasted view from the leader. A view
        % is delivered to the master process.
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group);
        stop ->
            ok
    end.

bcast(_Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).
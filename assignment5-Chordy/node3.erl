-module(node3).

-export([start/1, start/2, stabilize/4]).

-define(STABILIZE, 100).
-define(TIMEOUT, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, nil, storage:create()).

% connect to our successor
connect(Id, nil) ->
    {ok, {Id, nil, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            %% Here we should keep the reference
            Sref = erlang:monitor(process, Peer), 
            {ok, {Skey, Sref, Peer}}
    after ?TIMEOUT ->
        io:format("Timeout: no response from ~w~n", [Peer])
    end.



% 
schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

node(Id, Predecessor, Successor, Next, Store) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store);
        % a new node informs us of its existence
        {notify, New} ->
            {Pred, NewStore} = notify(New, Id, Predecessor, Store),
            % Here change predecessor
            node(Id, Pred, Successor, Next, NewStore);
        % a predecessor needs to know our predecessor
        % Peer ! {status, {Pkey, Ppid}}
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        % our successor informs us about its predecessor
        % it stays successor of us
        % or it become our Next and its predecessor becomes our successor
        {status, Pred, Nx} ->
            {Succ, ProperNext} = stabilize(Pred, Nx, Id, Successor),
            % Here change Successor
            node(Id, Predecessor, Succ, ProperNext, Store);
        stabilize ->
	        stabilize(Successor),
	        node(Id, Predecessor, Successor, Next, Store);
        %% The Qref params will be used to tag the return message to the Client
        % This allow the client to identify the reply message and makes
        % it easier to implement the client
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store);
        %% a new node should of course take over part of the responsibility
        % and must then also take over already added elements
        % This is only done when a node received and handles a notify message
        {handover, Elements} ->
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Next, Merged);
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Nxt, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        state ->
            io:format("ID: ~w~n", [Id]),
            io:format("Predecessor: ~p~n Successor: ~p~n Next: ~p~n", [Predecessor, Successor, Next]),
            io:format("Store: ~p~n", [Store]),
            node(Id, Predecessor, Successor, Next, Store);
        stop ->
            ok;
        UnknownMessage ->
            io:format("Unknown message type: ~p~n", [UnknownMessage])
    end.

% add a new key-value, we should first determine if our node is the node that
% should take care of the key. A node will take care of all keys from (but not
% including) the identifier of its predecessor to (and including) the identifier
% of itself....(successor takes care of the nodes)
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_ ,_ , Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

% determince if we are responsible for the key. If so we do a simple lookup in the local store
% and then send the reply to the requester.
lookup(Key, Qref, Client, Id, {Pkey, _, _Ppid}, {_Skey, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

create_probe(Id, {_Skey, _, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {_Skey, _, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = timer:now_diff(erlang:now(), T),
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

%% inform the peer of our current predecessor and our successor
request(Peer, Predecessor, Successor) ->
    {Skey, _, Spid} = Successor,
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = erlang:monitor(process, Npid),
            {{Nkey, Nref, Npid}, Keep};
        {Pkey, Pref, _Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    drop(Pref),
                    Nref = erlang:monitor(process, Npid),
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

% predecessor down
% set predecessor as nil
% someone will present themself as a possible predecessor
down(Ref, {_,Ref,_}, Successor, Next) ->
    {nil, Successor, Next};
% successor down
% monitor the next-node and stabilize
down(Ref, Predecessor, {_,Ref,_}, {Nkey, Npid}) ->
    Nref = erlang:monitor(process, Npid),
    self() ! stabilize,
    {Predecessor, {Nkey, Nref, Npid}, nil}.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    % only hand over the rest when a node received and handle a notify message
    Npid ! {handover, Rest},
    Keep.


stabilize({_Skey, _, Spid}) ->
    Spid ! {request, self()}.

% 当n加入np和ns中间时，建立双向链表关系
%  stabilize(Pred, Nx, Id, Successor)
stabilize(Pred, Next, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            % 
            %% Inform the node of our existence
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            %% It is pointing back to us, do nothing
            {Successor, Next};
        {Skey, _} ->
            %% It is pointing to itself, notify the node of our existence
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    %% Adopt that node as our successor and stabilize again
                    drop(Sref),
                    Xref = erlang:monitor(process, Xpid),
                    Xpid ! {request, self()},
                    %%% successor should be set to the new successor
                    self() ! stabilize,
                    % {sucessor, Next}
                    {{Xkey, Xref, Xpid}, Successor};
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.




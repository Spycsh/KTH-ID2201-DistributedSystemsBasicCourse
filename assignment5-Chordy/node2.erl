-module(node2).

-export([start/1, start/2]).

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
    node(Id, Predecessor, Successor, storage:create()).

% connect to our successor
connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?TIMEOUT ->
        io:format("Timeout: no response!~n")
    end.

% 
schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

node(Id, Predecessor, Successor, Store) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        % a new node informs us of its existence
        {notify, New} ->
            {Pred, NewStore} = notify(New, Id, Predecessor, Store),
            % Here change predecessor
            node(Id, Pred, Successor, NewStore);
        % a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        % our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            % Here change Successor
            node(Id, Predecessor, Succ, Store);
        stabilize ->
	        stabilize(Successor),
	        node(Id, Predecessor, Successor, Store);
        %% The Qref params will be used to tag the return message to the Client
        % This allow the client to identify the reply message and makes
        % it easier to implement the client
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        %% a new node should of course take over part of the responsibility
        % and must then also take over already added elements
        % This is only done when a node received and handles a notify message
        {handover, Elements} ->
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Merged);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        state ->
            io:format("ID: ~w~n", [Id]),
            io:format("Predecessor: ~p, Successor: ~p~n", [Predecessor, Successor]),
            io:format("Store: ~p~n", [Store]),
            node(Id, Predecessor, Successor, Store);
        stop ->
            ok
    end.

% add a new key-value, we should first determine if our node is the node that
% should take care of the key. A node will take care of all keys from (but not
% including) the identifier of its predecessor to (and including) the identifier
% of itself....(successor takes care of the nodes)
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

% determine if we are responsible for the key. If so we do a simple lookup in the local store
% and then send the reply to the requester.
lookup(Key, Qref, Client, Id, {Pkey, _Ppid}, {_Skey, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid!{lookup, Key, Qref, Client}
    end.

create_probe(Id, {_Skey, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {_Skey, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = timer:now_diff(erlang:now(), T),
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

% request is continously executed by schedule_stabilize
% it will send Peer the messages of a predecessor
% and the predecessor will judge whether it is a proper predecessor
% in stabilize/3
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    % only hand over the rest when a node received and handle a notify message
    Npid ! {handover, Rest},
    Keep.


stabilize({_Skey, Spid}) ->
    Spid ! {request, self()}.

% when n join between np and ns, establish the doubly linked list
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % initially the successor do not have a pred
            %% Inform the node of our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            %% It is pointing back to us, do nothing
            Successor;
        {Skey, Spid} ->
            % Pred is pointing back to itself (our successor's predecessor is our successor)
            %% It is pointing to itself, notify the node of our existence
            % Id<=Pred
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            % judge whether Id-> Xpid -> Spid
            % if true, then assign Xpid as Id's successor, stabilize again
            % if false, inform Spid myself (Id)
            case key:between(Xkey, Id, Skey) of
                true ->
                    %% Adopt that node as our successor and stabilize again
                    %% Xpid => Id
                    Xpid ! {request, self()},
                    Pred;
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.




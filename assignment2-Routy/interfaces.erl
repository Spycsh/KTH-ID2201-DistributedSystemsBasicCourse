-module(interfaces).
-export([new/0,add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

% A router will also need to keep track of a set of interfaces
% the record with each Name should be unique 
% the symbolic name (london), a process reference and a process
% identifier
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid}| lists:keydelete(Name,1,Intf)].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf);


% find the interferce Pid with name
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, _, Pid} -> {ok, Pid};
        _ -> notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, Ref, _} -> {ok, Ref};
        _ -> notfound
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, Ref, _} -> {ok, Name};
        _ -> notfound
    end.

% list all the link nodes, for updating
list(Intf) ->
    lists:map(fun({N,_,_}) -> N end, Intf).

broadcast(Message, Intf) ->
    lists:map(fun({_,_,C}) -> C ! Message end, Intf).


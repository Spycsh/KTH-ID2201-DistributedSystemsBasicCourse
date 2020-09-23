-module(dijkstra).
-export([route/2, table/2, iteration/3,iterate/3,update/4, entry/2, replace/4]).

% firstly filter out the nodes already are the gateways
% then purposefully endow other nodes with inf
% use iterate/3 to update them
table(Gateways, Map) ->
    RestNodes = lists:filter(fun(X)-> not lists:member(X, Gateways) end, map:all_nodes(Map)),
    DirectNodes = lists:map(fun(X) -> {X, 0, X} end, Gateways),
    IndeirectNodes = lists:map(fun(X)-> {X, inf, na} end, RestNodes),
    iterate(lists:append(DirectNodes, IndeirectNodes), Map, []).


route(Node, Table)->
    case lists:keyfind(Node, 1, Table) of
        {Node, Gw} ->
            {ok, Gw};
        _ ->
            notfound
    end.



% sorted list: [{berlin, 2, paris}, {berlin, 3, Shanghai}]
% the sorted list must NOT be duplicated.
iteration([], _, Table) ->
    Table;

iteration([F|R], Map, Table) ->
    case F of 
        {_,inf,_} -> Table;
        {FirstNode, N, Gw} ->                
            iteration(lists:foldl(fun(X, R1)->update(X,N+1,Gw,R1) end, R, map:reachable(FirstNode, Map)),
                    Map,
                    lists:keystore(FirstNode,1,Table,{FirstNode,Gw}))
    end.


iterate(Sorted, Map, Table) ->
    iteration(Sorted, Map, Table).


update(Node, N, Gateway, Sorted) ->
    M = entry(Node, Sorted),
    if 
        N<M->
            
            replace(Node, N, Gateway, Sorted);
        true->
            Sorted
    end.


% {berlin, 2, paris}
% returns the length of the shortest path to the
% node or 0 if the node is not found
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, L, _} ->
            L;
        false->
            0
    end.

replace(Node, N, Gateway, Sorted)->
    case lists:keyfind(Node, 1, Sorted) of
        {Node, _, _} ->
            lists:keysort(2, lists:keyreplace(Node,1,Sorted,{Node, N, Gateway}))
        end.
% replace(Node, N, Gw, Nodes) ->
%     insert({Node, N, Gw}, lists:keydelete(Node, 1, Nodes)).

%% Inserting a updated entry at the right position in the list.

% insert({Node, X, Gx}, [])->
%     [{Node, X, Gx}];
% insert({New, X, Gx}, [{Node, Y, Gy}|Rest])  when X < Y ->
%     [{New, X, Gx}, {Node, Y, Gy}|Rest];
% insert(New, [Node|Rest]) ->
%     [Node|insert(New, Rest)].




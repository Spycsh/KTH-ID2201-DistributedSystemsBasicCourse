-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

% updates the Map to reflect that Node
% has directional links to all nodes in the list Links. The old entry is
% removed.
% [{berlin,[london,paris]}]
update(Node, Links, Map)->
    case lists:keyfind(Node, 1, Map) of
        {_,_}->
            [{Node, Links}|lists:keydelete(Node,1,Map)];
        false ->
            [{Node, Links}|Map]
    end.


% returns the list of nodes directly reachable
% from Node
reachable(Node, Map) ->
    case lists:keyfind(Node, 1,Map) of
        {_,Link}->
            Link;
        false ->
            []
    end.

%  returns a list of all nodes in the map, also the ones
% without outgoing links
% usort: sort and delete the duplicated ones
% concat: [[a,b],[c,d]] => [a,b,c,d]
all_nodes(Map) ->
    lists:usort(lists:concat(lists:map(fun({N,L})->[N|L] end, Map))).


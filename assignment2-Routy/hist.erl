-module(hist).
-export([new/1, update/3]).

% history:  keeps track of what messages that we have seen
% for each messages should have a number

% return a new history where messages from Name will 
% always be seen as old
new(Node) ->
    [{Node, inf}].

update(Node, N, History)->
    case lists:keyfind(Node, 1, History) of
        {Node, MessageN} ->
            if 
                N > MessageN ->
                    {new, [{Node, N}|lists:keydelete(Node,1,History)]};
                true ->
                    old
            end;
        false ->
            {new, [{Node, N}|lists:keydelete(Node,1,History)]}
    end.



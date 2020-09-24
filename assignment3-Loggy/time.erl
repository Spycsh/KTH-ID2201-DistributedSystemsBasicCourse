-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% return the initial Lamport value
zero() ->
    0.

% return the time T incremented by one
% now ignore the Name which will be used later
inc(Name, T) ->
    T + 1.

% return the max of two time stamps
merge(Ti, Tj) ->
    if Ti < Tj ->
        Tj;
    true ->
        Ti
    end.

% Ti <= Tj, return true
leq(Ti, Tj) ->
    if Ti > Tj ->
        false;
    true ->
        true
    end.

% return a clock that can keep track of the nodes
clock([]) ->
    [];
clock([H|T]) ->
    [{H,0}|clock(T)].

% return a clock that has been updated
% given that we have received a log message 
% from a node at a given time
update(Node, Time, Clock) ->
    List = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    List.

% is it safe to log an event that happened at a given
% time, true or false
safe(_, []) ->
    true;
safe(Time, [{_, MsgTime}|T]) ->
    if
        Time =< MsgTime -> safe(Time, T);
        true -> false
    end.




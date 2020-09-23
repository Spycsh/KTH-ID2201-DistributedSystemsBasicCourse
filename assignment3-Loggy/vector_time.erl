-module(vector_time).
-export([zero/1, inc/3, merge/2, clock/2, update/3, leq/2,safe/2]).

%%% vector version
%%% [{Index, vector}, ..]
%%% [{p1, [0,0,0]}, {p2, [0,1,0]},...}]

zero(0) ->
    [];
zero(N) ->
    [0|zero(N-1)].

% increment a vector in the given process index
% input like: [0,0,0,...]
% N = 1
inc(_,[],_)->
    [];
inc(Index, [H|T], X) ->
    if X == Index ->
        [H+1|inc(Index, T, X+1)];
    true ->
        [H|inc(Index, T, X+1)]
    end.

% compare two vectors
leq(Vi, Vj) ->
    Comparison = lists:zipwith(fun(X, Y)->(Y>=X) end,Vi,Vj),
    Result = lists:all(fun(E)->E==true end,Comparison),
    Result.

% return the "max" of two vectors
merge(Vi, Vj) ->
    Res = leq(Vi, Vj),
    if Res -> Vj;
    true -> Vi
    end.



% input: a list of processes, N: number of processes
% return: [{john,[0,0,0,0]}, {paul,[0,0,0,0]}...]
clock([], _)->
    [];
clock([H|T], ZeroVect) ->
    % ZeroVect = zero(N),
    [{H, ZeroVect}|clock(T, ZeroVect)].


update(Node, TimeVector, Clock) ->
    List = lists:keyreplace(Node, 1, Clock, {Node, TimeVector}),
    List.


safe(_, []) ->
    true;
safe(TimeVector, [{_, MsgVector}|T]) ->
    Res = leq(MsgVector, TimeVector),
    if
        Res -> safe(TimeVector, T);
        true -> false
    end.
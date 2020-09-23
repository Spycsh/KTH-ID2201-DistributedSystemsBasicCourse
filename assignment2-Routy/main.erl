-module(main).
-export([main/0, broadcast/0, update/0, stopAll/0]).

% erl -name sweden@192.168.3.12 -setcookie routy -connect_all false

main() ->
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, goteborg),
    r1 ! {add, lund, {r2, 'sweden@192.168.3.12'}},
    r2 ! {add, goteborg, {r3, 'sweden@192.168.3.12'}},
    r3 ! {add, stockholm, {r1, 'sweden@192.168.3.12'}},
    true.

broadcast() ->
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    broadcasttrue.

update() ->
    r1 ! update,
    r2 ! update,
    r3 ! update,
    updatetrue.
    
stopAll() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
    stoptrue.
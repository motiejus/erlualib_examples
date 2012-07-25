-module(crutas_server_test).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    L = crutas_server:new(),
    ?assertEqual(ok, L:init()).

add_test_() -> [
        ?_assertEqual({ok, 4}, (crutas_server:new()):add(2, 2)),
        ?_assertEqual({ok, 0}, (crutas_server:new()):add(-1, 1))
    ].

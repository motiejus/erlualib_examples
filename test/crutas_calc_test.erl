-module(crutas_calc_test).

-include_lib("eunit/include/eunit.hrl").

add_test_() -> [
        ?_assertEqual({ok, 4}, crutas_calc:add(2, 2)),
        ?_assertEqual({ok, 0}, crutas_calc:add(-1, 1))
    ].

sin_test_() -> [
        ?_assertEqual({ok, 0}, crutas_calc:sin(0)),
        ?_assertEqual({ok, 1}, crutas_calc:sin(math:pi()/2))
    ].

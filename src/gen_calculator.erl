-module(gen_calculator).

-callback add(integer(), integer()) -> {ok, integer()}.
-callback sin(number()) -> {ok, number()}.

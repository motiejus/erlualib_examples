-module(gen_calculator).

-callback add(integer(), integer()) -> integer().
-callback sin(number()) -> number().

-module(gen_crutas).

-callback init() -> ok | stop.
-callback add(integer(), integer()) -> integer().

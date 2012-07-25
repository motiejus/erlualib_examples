-module(crutas_calc, [L]).
-behaviour(gen_calculator).
-include("paths.hrl").

-define(LUA_IMPL, ?PRIV_DIR(crutas) ++ "/crutas_calc.lua").
-export([sin/1, add/2, new/0]).

new() ->
    {ok, L} = lua:new_state(),
    {ok, Lua} = file:read_file(?LUA_IMPL),
    ok = lual:dostring(L, Lua),
    {?MODULE, L}.

add(Arg1, Arg2) ->
    luam:call(L, "add", [Arg1, Arg2]).

sin(Arg) ->
    luam:call(L, "sin", [Arg]).

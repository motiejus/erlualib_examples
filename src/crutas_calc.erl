-module(crutas_calc).
-behaviour(gen_calculator).
-include("paths.hrl").

-define(LUA_IMPL, ?PRIV_DIR(crutas) ++ "/crutas_calc.lua").
-export([sin/1, add/2]).

add(Arg1, Arg2) ->
    luam:one_call(?LUA_IMPL, "add", [Arg1, Arg2]).

sin(Arg) ->
    luam:one_call(?LUA_IMPL, "sin", [Arg]).

-module(crutas_server, [L]).
-behaviour(gen_crutas).
-include("paths.hrl").

-define(LUA_IMPL, ?PRIV_DIR(crutas) ++ "/crutas_server.lua").
-export([init/0, add/2, new/0]).

new() ->
    {ok, L} = lua:new_state(),
    {ok, Lua} = file:read_file(?LUA_IMPL),
    ok = lual:dostring(L, Lua),
    {?MODULE, L}.

init() ->
    luam:call(L, "init", []).

add(Arg1, Arg2) ->
    luam:call(L, "add", [Arg1, Arg2]).

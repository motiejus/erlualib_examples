-module(name_server).
-behaviour(gen_server).
-implemented_in({priv, "/name_server.lua"}).
-compile({parse_transform, lua_behaviour}).

%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
%-export([terminate/2]).
%
%init([]) ->
%    {ok, []}.
%
%handle_call({add_name, Name, Addr}, _, State) ->
%    {reply, ok, orddict:store(Name, Addr, State)};
%
%handle_call({get_addr, Addr}, _, State) ->
%    Ret = case orddict:find(Addr, State) of
%        {ok, Val} -> Val;
%        error -> error
%    end,
%    {reply, Ret, State}.
%
%handle_cast(_, State)    -> {stop, casting_kills, State}.
%handle_info(_, State)    -> {stop, gossip_kills,  State}.
%code_change(_, _, State) -> {ok, State}.
%terminate  (_, _)        -> ok.

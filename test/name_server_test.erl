-module(name_server_test).

-include_lib("eunit/include/eunit.hrl").

-define(P_NAME, <<"proper">>).
-define(P_ADDR, <<"proper.softlab.ntua.gr">>).
-define(M_ADDR, <<"m.jakstys.lt">>).

-define(SERVER, name_server).
-define(setup(T), {setup, fun start/0, fun stop/1,
                          fun(S) -> [fun() -> T(S) end] end}).

add_name_test_() -> [
        {"Wrong name", ?setup(fun wrong_name/1)},
        {"Add a new name and verify it's there", ?setup(fun add_name/1)},
        {"Add duplicate addresses", ?setup(fun dupl_addr/1)}
    ].

wrong_name(Pid) ->
    ?assertEqual(error, gen_server:call(Pid, {get_addr, ?P_NAME})).

add_name(Pid) ->
    ?assertEqual(ok, gen_server:call(Pid, {add_name, ?P_NAME, ?P_ADDR})),
    ?assertEqual(?P_ADDR, gen_server:call(Pid, {get_addr, ?P_NAME})),
    ?assertEqual(?P_ADDR, gen_server:call(Pid, {get_addr, ?P_NAME})).

dupl_addr(Pid) ->
    ?assertEqual(ok, gen_server:call(Pid, {add_name, ?P_NAME, ?P_ADDR})),
    ?assertEqual(ok, gen_server:call(Pid, {add_name, ?P_NAME, ?P_ADDR})),
    ?assertEqual(?P_ADDR, gen_server:call(Pid, {get_addr, ?P_NAME})).

start() -> {ok, Pid} = gen_server:start(?SERVER, [], []), Pid.
stop(Pid) -> exit(Pid, kill).

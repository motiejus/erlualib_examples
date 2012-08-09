-module(erlualib_examples_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlualib_examples_sup:start_link().

stop(_State) ->
    ok.

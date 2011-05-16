-module(ditto_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ditto_gui:start().
    %% Why would this app need a supervisor?
    %% ditto_sup:start_link().

stop(_State) ->
    ok.

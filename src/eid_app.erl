-module(eid_app).
-behaviour(application).

%% Callbacks
-export([start/2]).
-export([stop/1]).

%% Callbacks

start(_Type, _Args) ->
    eid_sup:start_link().

stop(_State) ->
    ok.

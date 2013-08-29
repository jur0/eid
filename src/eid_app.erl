-module(eid_app).
-behaviour(application).

%% Callbacks
-export([start/2]).
-export([stop/1]).

%% Callbacks

%% @private
start(_Type, _Args) ->
    eid_sup:start_link().

%% @private
stop(_State) ->
    ok.

-module(eid_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API

%% @doc Starts the main application supervisor.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% Callbacks

%% @private
init([]) ->
    Procs = [{eid_server, {eid_server, start_link, []},
              permanent, 5000, worker, [eid_server]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.

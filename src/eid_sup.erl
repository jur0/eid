%% Copyright (c) 2013-2019, Juraj Hlista <jurajhlista@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc API for starting the main supervisor.
-module(eid_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API

%% @doc Starts the main application supervisor.
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% Callbacks

%% @private
init([]) ->
    Procs = [{eid_server, {eid_server, start_link, []},
              permanent, 5000, worker, [eid_server]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.


%% @doc API for functions used in <code>eid_server</code> module.
-module(eid_utils).

%% API
-export([time_millis/0]).

%% API

%% @doc Returns the UNIX time stamp in milliseconds as an integer number.
-spec time_millis() -> pos_integer().
time_millis() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + erlang:trunc(MicroSecs / 1000).

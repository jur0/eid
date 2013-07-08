-module(eid).

%% API
-export([get_bin/0]).
-export([get_int/0]).

-include("eid.hrl").

%% @doc Returns unique id as a 64-bit binary. The upper 48 bits are a time
%% stamp (UNIX time stamp in milliseconds) and the lower 16 bits are a sequence
%% number.
-spec get_bin() -> bin_id().
get_bin() ->
    eid_server:get(bin).

%% @doc Returns an unique integer number. It is composed of the UNIX time stamp
%% in milliseconds and a sequence number.
-spec get_int() -> int_id().
get_int() ->
    eid_server:get(int).

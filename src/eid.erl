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

%% @doc API for retrieving ids.
-module(eid).

%% API
-export([get_bin/0]).
-export([get_bin/1]).
-export([get_int/0]).
-export([get_int/1]).
-export([max_seq/0]).

-type int_id() :: pos_integer().
-export_type([int_id/0]).

-type bin_id() :: << _:64 >>.
-export_type([bin_id/0]).

%% @doc Returns an unique id as a 64-bit binary. The upper 48 bits are a time
%% stamp (UNIX time stamp in milliseconds) and the lower 16 bits are a sequence
%% number. The id values are ascending.
-spec get_bin() -> {ok, bin_id()} | {error, term()}.
get_bin() ->
    eid_server:get({bin, ascend}).

%% @doc When called with 'ascend' parameter it returns the same as
%% {@link get_bin/0. <code>get_bin()</code>}. Although, when called with
%% 'descend' as parameter, it returns an id with inverted bits. The inversion
%% makes the ids descend.
-spec get_bin(ascend | descend) -> {ok, bin_id()} | {error, term()}.
get_bin(ascend) ->
    eid_server:get({bin, ascend});
get_bin(descend) ->
    eid_server:get({bin, descend}).

%% @doc Returns an unique id (such as {@link get_bin/0. <code>get_bin</code>}),
%% but represented as an integer number. The id values are ascending.
-spec get_int() -> {ok, int_id()} | {error, term()}.
get_int() ->
    eid_server:get({int, ascend}).

%% @doc When called with 'ascend' parameter it returns the same as
%% {@link get_int/0. <code>get_int()</code>}. Although, when called with
%% 'descend' as parameter, it returns an id with inverted bits. The inversion
%% makes the ids descend.
-spec get_int(ascend | descend) -> {int, int_id()} | {error, term()}.
get_int(ascend) ->
    eid_server:get({int, ascend});
get_int(descend) ->
    eid_server:get({int, descend}).

%% @doc Returns the maximal sequence number in integer format.
-spec max_seq() -> pos_integer().
max_seq() ->
    eid_server:max_seq().

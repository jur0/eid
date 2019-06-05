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

%% @doc API for a server where ids are generated.
-module(eid_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/1]).
-export([max_seq/0]).
-export([stop/0]).

%% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-record(state, {last_id :: eid:bin_id()}).

-define(SERVER, ?MODULE).
-define(MAX_ID, 16#ffffffffffffffff).
-define(MAX_SEQ, 16#ffff).

%% API

%% @doc Starts the gen_server. It stores the last time stamp in its state.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the gen_server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Returns either binary or integer unique id.
-spec get({int, ascend} | {int, descend} | {bin, ascend} | {bin, descend}) ->
        {ok, eid:int_id()} | {ok, eid:bin_id()} | {error, term()}.
get(Type) ->
    gen_server:call(?SERVER, {get, Type}).

%% @doc Return the maximal sequence number.
-spec max_seq() -> pos_integer().
max_seq() ->
    ?MAX_SEQ.

%% Callbacks

%% @private
init([]) ->
    {ok, Id} = id(0, eid_utils:time_millis(), 0),
    {ok, #state{last_id=Id}}.

%% @private
handle_call({get, {int, ascend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, eid_utils:time_millis(), Seq) of
        {ok, Id2} ->
            << Id3:64/integer >> = Id2,
            {{ok, Id3}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call({get, {int, descend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, eid_utils:time_millis(), Seq) of
        {ok, Id2} ->
            << Id3:64/integer >> = Id2,
            % bnot has higher priority than band
            Id4 = bnot Id3 band ?MAX_ID,
            {{ok, Id4}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call({get, {bin, ascend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, eid_utils:time_millis(), Seq) of
        {ok, Id2} ->
            {{ok, Id2}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call({get, {bin, descend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, eid_utils:time_millis(), Seq) of
        {ok, Id2} ->
            << Id3:64/integer >> = Id2,
            Id4 = bnot Id3 band ?MAX_ID,
            {{ok, << Id4:64/integer >>}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% Internal functions

%% @private
id(Time, Time2, _Seq) when Time =/= Time2 ->
    {ok, << Time2:48/integer, 0:16/integer >>};
id(Time, Time, Seq) when Seq < ?MAX_SEQ ->
    % The old and the new timestamp are the same, only increase Seq.
    {ok, << Time:48/integer, (Seq + 1):16/integer >>};
id(_Time, _Time, ?MAX_SEQ) ->
    {error, sequence_number_exceeded}.


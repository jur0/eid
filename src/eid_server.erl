-module(eid_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/1]).
-export([stop/0]).

%% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {last_id :: eid:bin_id()}).

-define(SERVER, ?MODULE).
-define(MAX_ID, 16#ffffffffffffffff).
-define(MAX_SEQ, 16#ffff).

%% API

%% @doc Starts the gen_server. It stores the last time stamp in its state.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the gen_server.
-spec stop() -> stopped.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Returns either binary or integer unique id.
-spec get({int, ascend} | {int, descend} | {bin, ascend} | {bin, descend}) ->
        {ok, eid:int_id()} | {ok, eid:bin_id()} | {error, term()}.
get(Type) ->
    gen_server:call(?SERVER, {get, Type}).

%% Callbacks

%% @private
init([]) ->
    {ok, Id} = id(eid_utils:time_millis(), 0),
    {ok, #state{last_id=Id}}.

%% @private
handle_call({get, {int, ascend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, Seq) of
        {ok, Id2} ->
            << Id3:64/integer >> = Id2,
            {{ok, Id3}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call({get, {int, descend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, Seq) of
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
    {Reply, Id} = case id(Time, Seq) of
        {ok, Id2} ->
            {{ok, Id2}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call({get, {bin, descend}}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >> = LastId}) ->
    {Reply, Id} = case id(Time, Seq) of
        {ok, Id2} ->
            << Id3:64/integer >> = Id2,
            Id4 = bnot Id3 band ?MAX_ID,
            {{ok, << Id4:64/integer >>}, Id2};
        {error, Reason} ->
            {{error, Reason}, LastId}
    end,
    {reply, Reply, #state{last_id=Id}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% @private
id(_Time, ?MAX_SEQ) ->
    {error, sequence_number_exceeded};
id(Time, Seq) ->
    Time2 = eid_utils:time_millis(),
    % if the current timestamp is the same as the previous one, the sequence
    % number is increased by 1
    Seq2 = case Time == Time2 of
        false ->
            0;
        true ->
            Seq + 1
    end,
    {ok, << Time2:48/integer, Seq2:16/integer >>}.

-module(eid_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0]).
-export([get/1]).

%% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("eid.hrl").

-record(state, {last_id :: bin_id()}).

-define(SERVER, ?MODULE).

%% API

%% Starts the gen_server.
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Stops the gen_server.
-spec stop() -> 'stopped'.
stop() ->
    gen_server:call(?SERVER, stop).

%% Returns either binary or integer unique id.
-spec get('int' | 'bin') -> int_id() | bin_id().
get(Type) ->
    gen_server:call(?SERVER, {get, Type}).

%% Callbacks

init([]) ->
    {ok, #state{last_id = id(eid_utils:time_millis(), 0)}}.

handle_call({get, int}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >>}) ->
    Id = id(Time, Seq),
    << IdInt:64/integer >> = Id,
    {reply, IdInt, #state{last_id = Id}};
handle_call({get, bin}, _From,
            #state{last_id = << Time:48/integer, Seq:16/integer >>}) ->
    Id = id(Time, Seq),
    {reply, Id, #state{last_id = Id}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

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
    << Time2:48/integer, Seq2:16/integer >>.

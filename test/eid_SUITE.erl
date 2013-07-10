-module(eid_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests
-export([int_id/1]).
-export([bin_id/1]).
-export([sequence_int_id/1]).
-export([sequence_max_int_id/1]).
-export([sequence_bin_id/1]).
-export([sequence_max_bin_id/1]).

%% NOTE: this must be the same as in eid_server!
-define(MAX_SEQ, 65535).

all() ->
    [
        {group, normal_id},
        {group, sequence_int_id},
        {group, sequence_max_int_id},
        {group, sequence_bin_id},
        {group, sequence_max_bin_id}
    ].

groups() ->
    NormalIdTests = [int_id, bin_id],
    SequenceIntIdTests = [sequence_int_id],
    SequenceMaxIntIdTests = [sequence_max_int_id],
    SequenceBinIdTests = [sequence_bin_id],
    SequenceMaxBinIdTests = [sequence_max_bin_id],
    [
        {normal_id, [sequence], NormalIdTests},
        {sequence_int_id, [sequence], SequenceIntIdTests},
        {sequence_max_int_id, [sequence], SequenceMaxIntIdTests},
        {sequence_bin_id, [sequence], SequenceBinIdTests},
        {sequence_max_bin_id, [sequence], SequenceMaxBinIdTests}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(normal_id, Config) ->
    ok = application:start(eid),
    Config;
init_per_group(Name, Config) when Name == sequence_int_id;
                                  Name == sequence_max_int_id;
                                  Name == sequence_bin_id;
                                  Name == sequence_max_bin_id ->
    ok = application:start(eid),
    ok = meck:new(eid_utils, [no_link]),
    ok = meck:expect(eid_utils, time_millis, fun() -> 0 end),
    Config.

end_per_group(normal_id, _Config) ->
    ok = application:stop(eid),
    ok;
end_per_group(Name, _Config) when Name == sequence_int_id;
                                  Name == sequence_max_int_id;
                                  Name == sequence_bin_id;
                                  Name == sequence_max_bin_id ->
    true = meck:validate(eid_utils),
    ok = meck:unload(eid_utils),
    ok = application:stop(eid),
    ok.

int_id(_Config) ->
    {ok, Id1} = eid:get_int(),
    {ok, Id2} = eid:get_int(),
    {ok, Id3} = eid:get_int(),
    {ok, Id4} = eid:get_int(),
    {ok, Id5} = eid:get_int(),
    true = Id1 < Id2,
    true = Id2 < Id3,
    true = Id3 < Id4,
    true = Id4 < Id5,
    ok.

bin_id(_Config) ->
    {ok, Id1} = eid:get_bin(),
    {ok, Id2} = eid:get_bin(),
    {ok, Id3} = eid:get_bin(),
    {ok, Id4} = eid:get_bin(),
    {ok, Id5} = eid:get_bin(),
    true = Id1 /= Id2,
    true = Id2 /= Id3,
    true = Id3 /= Id4,
    true = Id4 /= Id5,
    ok.

sequence_int_id(_Config) ->
    {ok, Id1} = eid:get_int(),
    {ok, Id2} = eid:get_int(),
    {ok, Id3} = eid:get_int(),
    {ok, Id4} = eid:get_int(),
    {ok, Id5} = eid:get_int(),
    Id1 = 0,
    Id2 = Id1 + 1,
    Id3 = Id2 + 1,
    Id4 = Id3 + 1,
    Id5 = Id4 + 1,
    ok.

sequence_max_int_id(_Config) ->
    lists:foreach(fun(X) ->
                {ok, Id} = eid:get_int(),
                true = X == Id end, lists:seq(0, ?MAX_SEQ)),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    ok.

sequence_bin_id(_Config) ->
    {ok, << Time1:48/integer, Seq1:16/integer >>} = eid:get_bin(),
    {ok, << Time2:48/integer, Seq2:16/integer >>} = eid:get_bin(),
    {ok, << Time3:48/integer, Seq3:16/integer >>} = eid:get_bin(),
    {ok, << Time4:48/integer, Seq4:16/integer >>} = eid:get_bin(),
    {ok, << Time5:48/integer, Seq5:16/integer >>} = eid:get_bin(),
    true = Time1 == Time2,
    true = Time2 == Time3,
    true = Time3 == Time4,
    true = Time4 == Time5,
    Seq1 = 0,
    Seq2 = 1,
    Seq3 = 2,
    Seq4 = 3,
    Seq5 = 4,
    ok.

sequence_max_bin_id(_Config) ->
    lists:foreach(fun(X) ->
                {ok, << _:48/integer, Seq:16/integer >>} = eid:get_bin(),
                true = X == Seq end, lists:seq(0, ?MAX_SEQ)),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    ok.

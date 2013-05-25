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
-export([sequence_bin_id/1]).

all() ->
    [
        {group, normal_id},
        {group, sequence_int_id},
        {group, sequence_bin_id}
    ].

groups() ->
    NormalIdTests = [int_id, bin_id],
    SequenceIntIdTests = [sequence_int_id],
    SequenceBinIdTests = [sequence_bin_id],
    [
        {normal_id, [sequence], NormalIdTests},
        {sequence_int_id, [sequence], SequenceIntIdTests},
        {sequence_bin_id, [sequence], SequenceBinIdTests}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(normal_id, Config) ->
    ok = application:start(eid),
    Config;
init_per_group(Name, Config) when Name == sequence_int_id;
                                  Name == sequence_bin_id ->
    ok = application:start(eid),
    meck:new(eid_utils, [no_link]),
    meck:expect(eid_utils, time_millis, fun() -> 0 end),
    Config.

end_per_group(normal_id, _Config) ->
    ok = application:stop(eid),
    ok;
end_per_group(Name, _Config) when Name == sequence_int_id;
                                  Name == sequence_bin_id ->
    meck:unload(eid_utils),
    ok = application:stop(eid),
    ok.

int_id(_Config) ->
    Id1 = eid:get_int(),
    Id2 = eid:get_int(),
    Id3 = eid:get_int(),
    Id4 = eid:get_int(),
    Id5 = eid:get_int(),
    true = Id1 < Id2,
    true = Id2 < Id3,
    true = Id3 < Id4,
    true = Id4 < Id5,
    ok.

bin_id(_Config) ->
    Id1 = eid:get_bin(),
    Id2 = eid:get_bin(),
    Id3 = eid:get_bin(),
    Id4 = eid:get_bin(),
    Id5 = eid:get_bin(),
    true = Id1 /= Id2,
    true = Id2 /= Id3,
    true = Id3 /= Id4,
    true = Id4 /= Id5,
    ok.

sequence_int_id(_Config) ->
    Id1 = eid:get_int(),
    Id2 = eid:get_int(),
    Id3 = eid:get_int(),
    Id4 = eid:get_int(),
    Id5 = eid:get_int(),
    Id1 = 0,
    Id2 = Id1 + 1,
    Id3 = Id2 + 1,
    Id4 = Id3 + 1,
    Id5 = Id4 + 1,
    ok.

sequence_bin_id(_Config) ->
    << Time1:48/integer, Seq1:16/integer >> = eid:get_bin(),
    << Time2:48/integer, Seq2:16/integer >> = eid:get_bin(),
    << Time3:48/integer, Seq3:16/integer >> = eid:get_bin(),
    << Time4:48/integer, Seq4:16/integer >> = eid:get_bin(),
    << Time5:48/integer, Seq5:16/integer >> = eid:get_bin(),
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

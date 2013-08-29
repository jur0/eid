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
-export([ascend_int_id/1]).
-export([ascend_bin_id/1]).
-export([descend_int_id/1]).
-export([descend_bin_id/1]).
-export([sequence_int_id/1]).
-export([sequence_max_int_id/1]).
-export([sequence_min_int_id/1]).
-export([sequence_bin_id/1]).
-export([sequence_max_bin_id/1]).
-export([sequence_min_bin_id/1]).

all() ->
    [
        {group, normal_id},
        {group, sequence_int_id},
        {group, sequence_max_int_id},
        {group, sequence_min_int_id},
        {group, sequence_bin_id},
        {group, sequence_max_bin_id},
        {group, sequence_min_bin_id}
    ].

groups() ->
    NormalIdTests = [ascend_int_id, descend_int_id, ascend_bin_id, descend_bin_id],
    SequenceIntIdTests = [sequence_int_id],
    SequenceMaxIntIdTests = [sequence_max_int_id],
    SequenceMinIntIdTests = [sequence_min_int_id],
    SequenceBinIdTests = [sequence_bin_id],
    SequenceMaxBinIdTests = [sequence_max_bin_id],
    SequenceMinBinIdTests = [sequence_min_bin_id],
    [
        {normal_id, [parallel], NormalIdTests},
        {sequence_int_id, [sequence], SequenceIntIdTests},
        {sequence_max_int_id, [sequence], SequenceMaxIntIdTests},
        {sequence_min_int_id, [sequence], SequenceMinIntIdTests},
        {sequence_bin_id, [sequence], SequenceBinIdTests},
        {sequence_max_bin_id, [sequence], SequenceMaxBinIdTests},
        {sequence_min_bin_id, [sequence], SequenceMinBinIdTests}
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
                                  Name == sequence_min_int_id;
                                  Name == sequence_bin_id;
                                  Name == sequence_max_bin_id;
                                  Name == sequence_min_bin_id ->
    ok = application:start(eid),
    ok = meck:new(eid_utils, [no_link]),
    ok = meck:expect(eid_utils, time_millis, fun() -> 0 end),
    Config.

end_per_group(normal_id, _Config) ->
    ok = application:stop(eid),
    ok;
end_per_group(Name, _Config) when Name == sequence_int_id;
                                  Name == sequence_max_int_id;
                                  Name == sequence_min_int_id;
                                  Name == sequence_bin_id;
                                  Name == sequence_max_bin_id;
                                  Name == sequence_min_bin_id ->
    true = meck:validate(eid_utils),
    ok = meck:unload(eid_utils),
    ok = application:stop(eid),
    ok.

ascend_int_id(_Config) ->
    Ids1 = get_ids(int, ascend, 100),
    Ids2 = get_ids(int, none, 100),
    Ids3 = get_ids(int, ascend, 100),
    ok = check_ids(ascend, Ids1 ++ Ids2 ++ Ids3),
    ok.

descend_int_id(_Config) ->
    Ids = get_ids(int, descend, 100),
    ok = check_ids(descend, Ids),
    ok.

ascend_bin_id(_Config) ->
    Ids1 = get_ids(bin, ascend, 100),
    Ids2 = get_ids(bin, none, 100),
    Ids3 = get_ids(bin, ascend, 100),
    ok = check_ids(ascend, Ids1 ++ Ids2 ++ Ids3),
    ok.

descend_bin_id(_Config) ->
    Ids = get_ids(bin, descend, 100),
    ok = check_ids(descend, Ids),
    ok.

sequence_int_id(_Config) ->
    Ids = get_ids(int, ascend, 100),
    ok = check_ids(ascend, Ids),
    ok.

sequence_max_int_id(_Config) ->
    Ids = get_ids(int, ascend, eid:max_seq()),
    ok = check_ids(ascend, Ids),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    ok.

sequence_min_int_id(_Config) ->
    Ids = get_ids(int, descend, eid:max_seq()),
    ok = check_ids(descend, Ids),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    {error, sequence_number_exceeded} = eid:get_int(),
    ok.

sequence_bin_id(_Config) ->
    Ids = get_ids(bin, ascend, 100),
    ok = check_ids(ascend, Ids),
    ok.

sequence_max_bin_id(_Config) ->
    Ids = get_ids(bin, ascend, eid:max_seq()),
    ok = check_ids(ascend, Ids),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    ok.

sequence_min_bin_id(_Config) ->
    Ids = get_ids(bin, descend, eid:max_seq()),
    ok = check_ids(descend, Ids),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    {error, sequence_number_exceeded} = eid:get_bin(),
    ok.

%% Internal functions

get_ids(int, none, Count) ->
    lists:map(fun(_X) ->
                {ok, Id} = eid:get_int(),
                Id end, lists:seq(0, Count));
get_ids(int, Type, Count) ->
    lists:map(fun(_X) ->
                {ok, Id} = eid:get_int(Type),
                Id end, lists:seq(0, Count));
get_ids(bin, none, Count) ->
    lists:map(fun(_X) ->
                {ok, Id} = eid:get_bin(),
                Id end, lists:seq(0, Count));
get_ids(bin, Type, Count) ->
    lists:map(fun(_X) ->
                {ok, Id} = eid:get_bin(Type),
                Id end, lists:seq(0, Count)).

% there must be at least 2 ids to check
check_ids(ascend, [Id1, Id2 | []]) when Id1 < Id2 ->
    ok;
check_ids(descend, [Id1, Id2 | []]) when Id1 > Id2 ->
    ok;
check_ids(Type, [Id1, Id2 | []]) ->
    ct:pal("Error, type: ~p, id1: ~p, id2: ~p~n", [Type, Id1, Id2]),
    error;
check_ids(ascend, [Id1, Id2 | Ids]) when Id1 < Id2 ->
    check_ids(ascend, [Id2 | Ids]);
check_ids(descend, [Id1, Id2 | Ids]) when Id1 > Id2 ->
    check_ids(descend, [Id2 | Ids]);
check_ids(Type, Ids) ->
    ct:pal("Error, type: ~p, ids: ~p~n", [Type, Ids]),
    error.

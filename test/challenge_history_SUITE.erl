%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十一月 2015 上午10:37
%%%-------------------------------------------------------------------
-module(challenge_history_SUITE).
-author("jie").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    Info = ?MODULE:module_info(),
    Functions = proplists:get_value(exports, Info),
    [FName || {FName, _} <- lists:filter(
        fun({module_info, _}) -> false;
            ({all, _}) -> false;
            ({init_per_suite, 1}) -> false;
            ({end_per_suite, 1}) -> false;
            ({_, 1}) -> true;
            ({_, _}) -> false
        end, Functions)].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    room:start_link(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_start(_) ->
    {ok, _} = challenge_history:start_link().

test_register(_) ->
    PlayerID = 1,
    Topic = 1,
    {ok, Pid} = challenge_history:start_link(),
    challenge_history:register(Pid, PlayerID, self(), 1, idle),
    challenge_history:register(Pid, PlayerID, self(), 2, idle),
    timer:sleep(10),
    [PlayerID] = challenge_history:match_same_topic(Pid, Topic),
    ok.

test_register_empty(_) ->
    PlayerID = 1,
    {ok, Pid} = challenge_history:start_link(),
    [] = challenge_history:match_same_topic(Pid, PlayerID),
    ok.

test_player_exit(_) ->
    PlayerID = 1,
    Topic = 1,
    {ok, Pid} = challenge_history:start_link(),
    Pid1 = spawn(?MODULE, player_process, []),
    challenge_history:register(Pid, PlayerID, Pid1, Topic, idle),

    timer:sleep(1),
    [PlayerID] = challenge_history:match_same_topic(Pid, Topic),

    exit(Pid1, kill),
    timer:sleep(10),
    [] = challenge_history:match_same_topic(Pid, Topic),
    ok.

test_same_topic_state(_) ->
    PlayerID1 = 1,
    PlayerID2 = 2,
    PlayerID3 = 3,
    Topic1 = 1,
    Topic2 = 2,

    {ok, Pid} = challenge_history:start_link(),
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    Pid3 = spawn(?MODULE, player_process, []),
    challenge_history:register(Pid, PlayerID1, Pid1, Topic1, idle),
    challenge_history:register(Pid, PlayerID1, Pid1, Topic2, idle),

    challenge_history:register(Pid, PlayerID2, Pid2, Topic1, idle),
    challenge_history:register(Pid, PlayerID3, Pid3, Topic1, play),

    timer:sleep(10),
    [PlayerID2, PlayerID1] = challenge_history:match_same_topic(Pid, Topic1),
    ok.

test_match_online_player_state(_) ->
    PlayerID1 = 1,
    PlayerID2 = 2,
    PlayerID3 = 3,
    Topic1 = 1,
    Topic2 = 2,

    {ok, Pid} = challenge_history:start_link(),
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    Pid3 = spawn(?MODULE, player_process, []),
    challenge_history:register(Pid, PlayerID1, Pid1, Topic1, idle),
    challenge_history:register(Pid, PlayerID1, Pid1, Topic2, idle),

    challenge_history:register(Pid, PlayerID2, Pid2, Topic1, idle),
    challenge_history:register(Pid, PlayerID3, Pid3, Topic1, play),

    timer:sleep(10),
    [PlayerID2, PlayerID1] = challenge_history:match_online_player(Pid),
    ok.

player_process() ->
    timer:sleep(infinity).

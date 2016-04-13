%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十一月 2015 下午1:56
%%%-------------------------------------------------------------------
-module(room_SUITE).
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
    {ok, _} = challenge_history:start_link(),
    company:start_link(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.


test_create(_) ->
    GroupID = 1,
    Company = "ND",
    Topic   = history,
    company:create(GroupID),
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    timer:sleep(100),
    RoomID = room:create(PlayerID1, Pid1, PlayerID2, Pid2, GroupID, Company, Topic),
    Member = room:get_member(RoomID),
    2 = length(Member).

test_player_exit(_) ->
    GroupID = 1,
    Company = "ND",
    Topic   = history,
    company:create(GroupID),
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    timer:sleep(100),
    RoomID = room:create(PlayerID1, Pid1, PlayerID2, Pid2, GroupID, Company, Topic),
    erlang:exit(Pid1, shutdown),
    timer:sleep(100),
    Member = room:get_member(RoomID),
    1 = length(Member),

    erlang:exit(Pid2,shutdown),
    timer:sleep(100),
    [] = ets:lookup(room, RoomID).

test_player_exit2(_) ->
    GroupID = 1,
    Company = "ND",
    Topic   = history,
    company:create(GroupID),
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    timer:sleep(100),
    RoomID = room:create(PlayerID1, Pid1, PlayerID2, Pid2, GroupID, Company, Topic),
    room:exit(RoomID, PlayerID1, Pid1),
    Member = room:get_member(RoomID),
    1 = length(Member),

    room:exit(RoomID, PlayerID2, Pid2),
    timer:sleep(100),
    [] = ets:lookup(room, RoomID),
    [] = ets:lookup(player_room, Pid1),
    [] = ets:lookup(player_room, Pid2).

player_process() ->
    timer:sleep(infinity).



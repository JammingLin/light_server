%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十一月 2015 下午3:29
%%%-------------------------------------------------------------------
-module(question_SUITE).
-author("linzhiwei").

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
    tplt_data:start_link("../../../data/template/"),
    m_rand:start_link(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_get_room_id(_)->
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    RoomID = room:create(PlayerID1, Pid1, PlayerID2, Pid2, history),
    RoomID = room:get_room_id(Pid1).

test_start_first_round(_)->
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    RoomID = room:create(PlayerID1, Pid1, PlayerID2, Pid2, history),

    game_logic:enter_game(RoomID, PlayerID1),
    game_logic:enter_game(RoomID, PlayerID2),
    Status = 1,

    Status = game_logic:get_cur_status(RoomID).

test_multify_room(_)->
    PlayerID1 = 1,
    PlayerID2 = 2,
    Pid1 = spawn(?MODULE, player_process, []),
    Pid2 = spawn(?MODULE, player_process, []),
    RoomID1 = 1,
    RoomID1 = room:create(PlayerID1, Pid1, PlayerID2, Pid2, history),

    PlayerID3 = 3,
    PlayerID4 = 4,
    Pid3 = spawn(?MODULE, player_process, []),
    Pid4 = spawn(?MODULE, player_process, []),
    RoomID2 = 2,
    RoomID2 = room:create(PlayerID3, Pid3, PlayerID4, Pid4, history).

player_process() ->
    timer:sleep(infinity).


%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2015 上午9:59
%%%-------------------------------------------------------------------
-module(db_timer_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
        fun ({module_info,_}) -> false;
            ({all,_}) -> false;
            ({init_per_suite,1}) -> false;
            ({end_per_suite,1}) -> false;
            ({_,1}) -> true;
            ({_,_}) -> false
        end, Functions)].

init_per_suite(Config) ->
    application:set_env(db, db_module, db_mysql),
    ok = application:start(db),
    init_table(),
    db_helper:load_mysql_schema(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(db),
    ok.

init_per_group(_group, Config) ->
    Config.

end_per_group(_group, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

init_table() ->
    Query1 = "CREATE TABLE IF NOT EXISTS `one_primary` (
  `id` int(11) NOT NULL,
  `age` int(11) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;",
    db:execute2(Query1),
    Query2 = "CREATE TABLE IF NOT EXISTS `two_primary` (
      `player_id` int(11) NOT NULL,
      `item_id` int(11) NOT NULL,
      `age` int(11) NOT NULL,
      PRIMARY KEY (`player_id`, `item_id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8;",
    db:execute2(Query2).

test_delete_all(_) ->
    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),
    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),

    db_timer:delete_all(Table1),
    [] = db_timer:get(Table1, 1),
    db_timer:delete_all(Table2),
    [] = db_timer:get(Table2, 1),
    [] = db_timer:get(Table2, 1, 1),

    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 2, age => 2}),
    db_timer:delete(Table1, 1),
    db_timer:delete(Table2, 1),
    [] = db_timer:get(Table1, 1),
    [] = db_timer:get(Table2, 1),
    [] = db_timer:get(Table2, 1, 1),
    [] = db_timer:get(Table2, 1, 2),

    db_timer:delete_all(Table1),
    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table1, #{id => 2, age => 2}),
    gen_server:call(db_timer, swap_for_test), %% 交换缓存, 测试双缓冲的情况下, 针对相同的主键, 是否有问题
    db_timer:delete(Table1, 1),
    db_timer:delete(Table1, 2),
    RR3 = #{id => 3, age => 3},
    RR4 = #{id => 4, age => 4},
    db_timer:save(Table1, RR3),
    db_timer:save(Table1, RR4),
    [] = db_timer:get(Table1, 1),
    [] = db_timer:get(Table1, 2),
    [RR3] = db_timer:get(Table1, 3),
    [RR4] = db_timer:get(Table1, 4),

    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 2, age => 2}),
    gen_server:call(db_timer, swap_for_test), %% 交换缓存, 测试双缓冲的情况下, 针对相同的主键, 是否有问题
    db_timer:delete(Table2, 1, 2),
    db_timer:delete(Table2, 1),
    R3 = #{player_id => 1, item_id => 3, age => 3},
    R4 = #{player_id => 1, item_id => 4, age => 4},
    db_timer:save(Table2, R3),
    db_timer:save(Table2, R4),
    [] = db_timer:get(Table2, 1, 1),
    [] = db_timer:get(Table2, 1, 2),
    [R3] = db_timer:get(Table2, 1, 3),
    [R4] = db_timer:get(Table2, 1, 4),
    utils:check_list([R3, R4], db_timer:get(Table2, 1)),
    ok.

test_get_one(_) ->
    Table1 = one_primary,
    db_timer:delete_all(Table1),

    R1 = #{id => 1, age => 1},
    db_real:save(Table1, R1),
    R11 = R1#{age := 2},
    db_timer:save(Table1, R11),
    [R11] = db_timer:get(Table1, 1),
    db_timer:delete(Table1, 1),
    [] = db_timer:get(Table1, 1),
    ok.

test_get_multi(_) ->
    Table1 = two_primary,
    db_timer:delete_all(Table1),

    R1 = #{player_id => 1, item_id => 1, age => 1},
    R2 = #{player_id => 1, item_id => 2, age => 2},
    R3 = #{player_id => 1, item_id => 3, age => 3},
    R4 = #{player_id => 1, item_id => 4, age => 4},
    [db_real:save(Table1, R) || R <- [R1, R2, R3, R4]],

    db_timer:delete(Table1, 1, 1),
    R21 = R2#{age := 21},
    db_timer:save(Table1, R21),

    R5 = #{player_id => 1, item_id => 5, age => 5},
    R6 = #{player_id => 1, item_id => 6, age => 6},
    db_timer:save(Table1, R5),
    db_timer:save(Table1, R6),
    db_timer:delete(Table1, 1, 6),
    utils:check_list([R21, R3, R4, R5], db_timer:get(Table1, 1)),
    ok.

test_save_to_db(_) ->
    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    db_timer:save(Table1, R1),
    R11 = R1#{age := 11},
    db_timer:save(Table1, R11),

    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    R22 = R2#{age := 3},
    db_timer:save(Table2, R2),
    db_timer:save(Table2, R22),

    db_timer:save_to_db_for_test(),

    [R11] = db_real:get(Table1, ID),
    [R22] = db_real:get(Table2, PlayerID),
    ok.

test_save_timer(_) ->
    TimeToSave = 500,
    Pid = whereis(db_timer),
    db_timer:start_timer_for_test(Pid, TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    db_timer:save(Table1, R1),

    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    db_timer:save(Table2, R2),

    timer:sleep(TimeToSave div 2),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID),

    timer:sleep(TimeToSave),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID),
    ok.

test_timer_swap(_) ->
    TimeToSave = 500,
    Pid = whereis(db_timer),
    db_timer:start_timer_for_test(Pid, TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    db_timer:save(Table1, R1),
    db_timer:save(Table2, R2),

    timer:sleep(TimeToSave + 200),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID),

    R11 = R1#{age:=2},
    R22 = R2#{age:=3},
    db_timer:save(Table1, R11),
    db_timer:save(Table2, R22),

    timer:sleep(TimeToSave + 200),
    [R11] = db_real:get(Table1, ID),
    [R22] = db_real:get(Table2, PlayerID),
    ok.

test_delete(_) ->
    TimeToSave = 500,
    Pid = whereis(db_timer),
    db_timer:start_timer_for_test(Pid, TimeToSave),

    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    R3 = #{player_id => PlayerID, item_id => 3, age => 3},
    db_real:save(Table1, R1),
    db_real:save(Table2, R2),
    db_real:save(Table2, R3),

    db_timer:delete(Table1, ID),
    db_timer:delete(Table2, PlayerID, 2),
    timer:sleep(TimeToSave + 200),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID, 2),
    [R3] = db_real:get(Table2, PlayerID, 3),
    ok.

test_save_delete(_) ->
    TimeToSave = 500,
    Pid = whereis(db_timer),
    db_timer:start_timer_for_test(Pid, TimeToSave),

    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},

    db_timer:save(Table1, R1),
    db_timer:delete(Table1, ID),
    db_timer:save(Table2, R2),
    db_timer:delete(Table2, PlayerID),
    timer:sleep(TimeToSave + 200),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID, 2),
    [] = db_real:get(Table2, PlayerID),
    ok.

test_delete_save(_) ->
    TimeToSave = 500,
    Pid = whereis(db_timer),
    db_timer:start_timer_for_test(Pid, TimeToSave),

    Table1 = one_primary,
    Table2 = two_primary,
    db_timer:delete_all(Table1),
    db_timer:delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},

    db_timer:delete(Table1, ID),
    db_timer:save(Table1, R1),
    db_timer:delete(Table2, PlayerID),
    db_timer:save(Table2, R2),
    timer:sleep(TimeToSave + 200),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID, 2),
    [R2] = db_real:get(Table2, PlayerID),
    ok.


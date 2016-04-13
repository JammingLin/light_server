%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2015 下午5:19
%%%-------------------------------------------------------------------
-module(cache_SUITE).
-author("jie").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 200}}].

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
    ok = application:start(db),
    init_table(),
    db_helper:load_mysql_schema(),
    cache:start_link(),
    Config.

end_per_suite(_Config) ->
    ok.

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

test_get_empty(_) ->
    [] = cache:get(one_primary, 2).

test_get_save(_) ->
    Table = one_primary,
    R = #{id => 1, age => 1},
    cache:save(Table, R),
    R = cache:get(Table, 1).


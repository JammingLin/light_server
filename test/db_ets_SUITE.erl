%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十一月 2015 下午3:37
%%%-------------------------------------------------------------------
-module(db_ets_SUITE).
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
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_db_get(_)->
    ok.

test_db_delete(_)->
    ok.

test_db_add(_)->
    ok.

test_db_update(_)->
    ok.

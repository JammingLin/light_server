%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十一月 2015 下午3:37
%%%-------------------------------------------------------------------
-module(company_SUITE).
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
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_start(_) ->
    {ok, _} = company:start_link().

test_create(_) ->
    GroupID = 1,
    {ok, _} = company:start_link(),
    company:create(GroupID),
    timer:sleep(1),
    Pid = company:get_challenge_history_pid(GroupID),
    true = is_pid(Pid).

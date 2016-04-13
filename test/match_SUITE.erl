%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十一月 2015 下午12:02
%%%-------------------------------------------------------------------
-module(match_SUITE).
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
    {ok, _} = challenge_history:start_link(),
    company:start_link(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

test_match_pool_null(_)->
    Company = "xmnd",
    Topic   = "1",
    init_company_data(),
    [] = topic_pool:get_match_player_list(Company, Topic).

test_search_match_pool(_)->
    Company = "xmnd",
    Topic   = 1,
    GroupID = 1,
    init_company_data(),
    PlayerID1 = 1,
    Pid1 = spawn(?MODULE, player_process, []),
    TopicPid = company:get_topic_pid(GroupID, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID1, Pid1, 1),
    [] = topic_pool:get_match_player_list(Company, Topic).

test_join_match_pool(_)->
    Company = "xmnd",
    Topic   = 1,
    GroupID = 1,
    init_company_data(),
    PlayerID1 = 1,
    Pid1 = spawn(?MODULE, player_process, []),
    PlayerID2 = 2,
    Pid2 = spawn(?MODULE, player_process, []),
    TopicPid = company:get_topic_pid(GroupID, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID1, Pid1, 1),
    [] = topic_pool:get_match_player_list(Company, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID2, Pid2, 1),
    timer:sleep(100),
    [{Pid2, PlayerID2,_}] = topic_pool:get_match_player_list(Company, Topic).

test_player_exit(_)->
    Company = "xmnd",
    Topic   = 1,
    GroupID = 1,
    init_company_data(),
    PlayerID1 = 1,
    Pid1 = spawn(?MODULE, player_process, []),
    TopicPid = company:get_topic_pid(GroupID, Topic),
    [] = topic_pool:get_match_player_list(Company, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID1, Pid1, 1),
    timer:sleep(100),
    [{Pid1, PlayerID1,_}] = topic_pool:get_match_player_list(Company, Topic),
    erlang:exit(Pid1, shutdown),
    timer:sleep(100),
    [] = topic_pool:get_match_player_list(Company, Topic).

test_player_agree_pk(_)->
    Company = "xmnd",
    Topic   = 1,
    GroupID = 1,
    init_company_data(),
    PlayerID1 = 1,
    Pid1 = spawn(?MODULE, player_process, []),
    PlayerID2 = 2,
    Pid2 = spawn(?MODULE, player_process, []),
    TopicPid = company:get_topic_pid(GroupID, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID1, Pid1, 1),
    [] = topic_pool:get_match_player_list(Company, Topic),
    topic_pool:join_topic_pool(TopicPid,GroupID, PlayerID2, Pid2, 1),
    timer:sleep(100),
    [{Pid2, PlayerID2,_}] = topic_pool:get_match_player_list(Company, Topic).

player_process() ->
    timer:sleep(infinity).

init_company_data()->
    GroupID = 1,
    Company = "xmnd",
    Topic   = 1,
    company:create(GroupID),
    {ok, _} = topic_pool:start_link(Company, GroupID, Topic).
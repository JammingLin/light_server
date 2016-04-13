%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2014 16:09
%%%-------------------------------------------------------------------
-module(db_update_config).
-author("long").

%% API
-export([get/0]).

get() ->
    %% 下面这些配置只能不停得往最后面添加, 不能在中间添加, 否则数据库升级会出问题
    %% 即使要回滚数据或者数据库结构, 也需要通过写新的sql语句来实现
    %% 备注: 如果是本机测试的时候发现sql有问题(就是代码还没提交到svn上),
    %% 可以简单的删除db_update表中的对应记录及其这记录之后的所有记录, 这样数据库会重新升级
    [
        {"create table `server_id`",     "create_table_server_id.sql"},
        {"create table `id_generator`",  "create_table_id_generator.sql"},
        {"create table mock_hash_table", "create_table_mock_hash_table.sql"},
        {"create table `passport`",      "create_table_passport.sql"},
        {"create table `player`",        "create_table_player.sql"},
        {"create table player_status",   "create_table_player_status.sql"},
        {"create table player feedback", "create_table_player_feedback.sql"}
    ].
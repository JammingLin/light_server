%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2014 19:48
%%%-------------------------------------------------------------------
-module(db_update).
-author("long").

%% API
-export([start/1, start/2]).

start(Pool) ->
    start(Pool, "data/sql/").

start(Pool, Path) ->
    [begin
         Host = proplists:get_value(host, Config),
         Database = proplists:get_value(database, Config),
         %% error_logger:info_msg("database(~p (~p)) updating ......~n", [Host, Database]),
         lager:info("database(~p (~p)) updating ......", [Host, Database]),
         create_update_history_table(PoolName),
         migrate(PoolName, Path)
     end || {PoolName, Config} <- Pool],
    %% error_logger:info_msg("database update finish~n").
    lager:info("database update finish").

create_update_history_table(PoolName) ->
    Sql = "CREATE TABLE IF NOT EXISTS `update_history` (
                      `id` int(11) NOT NULL auto_increment,
                      `sql_command` varchar(16000) NOT NULL,
                      `comment` varchar(255) not null,
                      `migrate_time` datetime not null,
                      PRIMARY KEY (`id`)
                    ) ENGINE=InnoDB DEFAULT CHARSET=utf8;",
    db_mysql_common:execute(PoolName, Sql).

migrate(PoolName, Path) ->
    Sql = "select count(*) as cnt from `update_history`",
    [Record] = db_mysql_common:query(PoolName, Sql),
    Count = maps:get(cnt, Record),
    Migrations = db_update_config:get(),
    Files = get_update_sql_file(Count, Migrations),
    migrate_impl(PoolName, Path, Files).

get_update_sql_file(Count, Migrations) ->
    {_, Files} = lists:split(Count, Migrations),
    Files.

migrate_impl(_PoolName, _Path, []) ->
    ok;
migrate_impl(PoolName, Path, [{Comment, File} | Rest]) ->
    execute_sql_file(PoolName, Path, Comment, File),
    migrate_impl(PoolName, Path, Rest).

execute_sql_file(PoolName, Path, Comment, FileName) ->
    {ok, Binary} = file:read_file(Path ++ FileName),
    BatchSql = binary_to_list(Binary),
    Sqls = string:tokens(BatchSql, ";"),
    lists:foreach(fun(Sql) ->
        Sql1 = string:strip(Sql, both, $\n),
        Sql2 = string:strip(Sql1, both, $\r),
        Sql3 = string:strip(Sql2, both, $ ),
        Sql4 = string:strip(Sql3, both, $\t),
        case Sql4 of
            [] -> ok;
            Empty when length(Empty) < 10 -> ok;
            _  ->
                case db_mysql_common:execute2(PoolName, Sql4) of
                    {updated, _Result} ->
                        ok;
                    DBR ->
                        io:format("sql:~p~n", [Sql]),
                        io:format("sql4:~p~n", [Sql4]),
                        throw({Sql, Sql4, DBR})
                end
        end
    end, Sqls),
    insert_update_history(PoolName, Sqls, Comment).

insert_update_history(PoolName, Sql, Comment) ->
    UpdateHistory = lists:concat([
        "insert into `update_history` set sql_command = '", Sql, "',
        comment = '", Comment, "',
        migrate_time = '", datetime:datetime_to_string(datetime:localtime()), "'"]),
    %% 数据库升级失败的处理, 应该让整个程序都不能启动, 直到修复完成为止
    {updated, _} = db_mysql_common:execute2(PoolName, UpdateHistory).



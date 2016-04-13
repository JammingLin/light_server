%%%-------------------------------------------------------------------
%%% @author chendonghua
%%% @copyright (C) 2015, nd
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2015 15:22
%%%-------------------------------------------------------------------
-module(cache_manager).
-author("chendonghua").
-define(CACHE_MODULE, cache_redis). %% TODO: 实际上需要与 db_cache 里的配置一致

%% API
-export([]).
-compile(export_all).

start() ->
    LastSchema = get_last_mysql_schema(),
    CurrentSchema = get_current_mysql_schema(),
    CheckResult = check_schema(LastSchema, CurrentSchema),
    clear_cache(CheckResult),
    update_last_mysql_schema(CurrentSchema),
    ok.

get_last_mysql_schema() ->
    Rows = db:get(mock_hash_table, "last_mysql_schema"),
    [case is_atom(TableName) of
         true ->
             {TableName, utils:binary_to_data(Value)};
         false ->
             {binary_to_atom(TableName, utf8), utils:binary_to_data(Value)}
     end || #{hfield:=TableName, hvalue:=Value} <- Rows].

get_current_mysql_schema() ->
    ets:tab2list(mysql_schema).

update_last_mysql_schema(CurrentSchema) ->
    db:delete(mock_hash_table, "last_mysql_schema"),
    [db:save(mock_hash_table, #{hkey=>"last_mysql_schema", hfield=> Table, hvalue=> utils:data_to_binary(Info)})
        || {Table, Info} <- CurrentSchema].

check_schema(LastSchema, CurrentSchema) ->
    [TableName || {TableName, Info} <- LastSchema,
        length([1 || {TableNameC, _} <- CurrentSchema, TableNameC =:= TableName]) == 0 orelse %% 删除的表
        length([1 || {TableNameC, InfoC} <- CurrentSchema, TableNameC =:= TableName, InfoC =/= Info]) > 0]. %% 变更的表

clear_cache(CheckResult) when is_list(CheckResult) ->
    [delete_cache(Table) || Table <- CheckResult].

delete_cache(TableName) ->
    ?CACHE_MODULE:no_trans_delete(TableName).



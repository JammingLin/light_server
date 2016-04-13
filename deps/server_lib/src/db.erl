%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 八月 2014 9:52
%%%-------------------------------------------------------------------
-module(db).
-author("Administrator").

-include_lib("emysql.hrl").

%% API
-export([start_link/0, start_link/1, get_primary_name/1, get_field_type/2]).
-export([get/2, get/3]).
-export([save/2]).
-export([delete/2, delete/3, delete_all/1, make_delete_stmt/2]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([encode/1]).

start_link() ->
    db_proxy:start_link([]).

start_link(DBNode) ->
    db_proxy:start_link(DBNode).

-spec get_primary_name(Table::atom()) -> [atom()].
%% 获得指定表的主键名
get_primary_name(Table) when is_atom(Table) ->
    [{_Tab, ColumnInfo}] = ets:lookup(mysql_schema, Table),
    PrimaryNameList = maps:get(primary_name_list, ColumnInfo),
    PrimaryNameList.

-spec get_field_type(Table::atom(), FieldName::atom()) -> atom().
%% 获得指定字段对应的数据类型
get_field_type(Table, FieldName) ->
    [{_Tab, ColumnInfo}] = ets:lookup(mysql_schema, Table),
    Columns = maps:get(columns, ColumnInfo),
    {_, Type} = lists:keyfind(FieldName, 1, Columns),
    Type.

%% 通过主键值和表获得对应的纪录
-spec get(TableName::atom(), PrimaryValue::any()) -> map().
get(TableName, PrimaryValue) when is_atom(TableName) ->
    db_proxy:get(TableName, PrimaryValue).

%% 通过指定两个主键字段值获得对应的纪录
-spec get(TableName::atom(), PrimaryValue1::any(), PrimaryValue2::any()) -> [map()].
get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    db_proxy:get(TableName, PrimaryValue1, PrimaryValue2).

-spec save(TableName::atom(), Record::map()) -> ok.
%% 把Record保存到对应的表中, Record是以map的形式存在
save(TableName, Record) when is_atom(TableName), is_map(Record) ->
    db_proxy:save(TableName, Record).

-spec delete(TableName::atom(), Record::map()) -> ok.
%% 从数据库中删除指定Record
delete(TableName, PrimaryValue) when is_atom(TableName) ->
    db_proxy:delete(TableName, PrimaryValue).

delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    db_proxy:delete(TableName, PrimaryValue1, PrimaryValue2).

delete_all(TableName) when is_atom(TableName) ->
    %% TODO: 如何改造该函数， 以适应分布式数据库的请求
    db_proxy:delete_all(TableName).

make_delete_stmt(TableName, Record) ->
    case get_primary_name(TableName) of
        [PrimaryName] ->
            PrimaryValue = maps:get(PrimaryName, Record),
            db_real:make_delete_stmt(TableName, PrimaryName, PrimaryValue);
        [PrimaryName1, PrimaryName2] ->
            PrimaryValue1 = maps:get(PrimaryName1, Record),
            PrimaryValue2 = maps:get(PrimaryName2, Record),
            db_real:make_delete_stmt(TableName, PrimaryName1, PrimaryName2, PrimaryValue1, PrimaryValue2)
    end.

encode(Val) ->
    Val1 = translate:to_mysql(Val),
    mysql:encode(Val1).

%% 执行insert, delete, update等操作, 返回受到影响的行数
execute(Query) ->
    db_proxy:execute(Query).

%% 执行insert, delete, update等操作, 返回受到影响的行数
execute(Key, Query) ->
    db_proxy:execute(Key, Query).

%% 执行insert, delete, update等操作, 返回ok_packet
execute2(Query) ->
    db_proxy:execute2(Query).

%% 执行insert, delete, update等操作, 返回ok_packet
execute2(Key, Query) ->
    db_proxy:execute2(Key, Query).

query(Query) ->
    db_proxy:query(Query).

query(Key, Query) ->
    db_proxy:query(Key, Query).

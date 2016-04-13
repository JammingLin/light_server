%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%  实时向数据库中写入数据,和db_timer形成对比
%%% @end
%%% Created : 02. Apr 2015 下午2:22
%%%-------------------------------------------------------------------
-module(db_real).
-author("jie").

-export([start_link/0, stop/0]).
-export([get/2, get/3]).
-export([save/2]).
-export([delete/2, delete/3, delete_all/1]).
-export([make_delete_stmt/3, make_delete_stmt/5]).


start_link() ->
    {ok, spawn(fun() ->
        receive
            loop -> ok
        end
               end)}.

stop() ->
    ok.

-spec get(TableName::atom(), PrimaryValue::any()) -> map().
get(TableName, PrimaryValue) when is_atom(TableName) ->
    [PrimaryName|_] = db:get_primary_name(TableName),
    Query = lists:concat(["select * from ", TableName, " where ", PrimaryName, " = ", db:encode(PrimaryValue)]),
    db_helper:query(PrimaryValue, Query).

-spec get(TableName::atom(), PrimaryValue1::any(), PrimaryValue2::any()) -> [map()].
get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    [PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
    Query = lists:concat(["select * from ", TableName,
        " where ", PrimaryName1, "=", db:encode(PrimaryValue1)," and ", PrimaryName2, "=", db:encode(PrimaryValue2)]),
    db_helper:query(PrimaryValue1, Query).


-spec save(TableName :: atom(), Record :: map()) -> Result | [Result].
save(TableName, Record) when is_atom(TableName), is_map(Record) ->
    F = fun(Value) ->
        case is_list(Value) of
            true -> list_to_binary(Value);
            false -> Value
        end
    end,

    case db:get_primary_name(TableName) of
        [Name] ->
            L = [{Field, F(Value)} || {Field, Value} <- maps:to_list(Record), Field /= Name],
            FieldValueList = lists:flatten(L),
            Value = maps:get(Name, Record),
            save(TableName, Value, FieldValueList);
        [Name1, Name2] = NameList ->
            L = [{Field, F(Value)} || {Field, Value} <- maps:to_list(Record), not lists:member(Field, NameList)],
            FieldValueList = lists:flatten(L),
            Value1 = maps:get(Name1, Record),
            Value2 = maps:get(Name2, Record),
            save(TableName, Value1, Value2, FieldValueList)
    end.


-spec delete(TableName :: atom(), Record :: map()) -> Result | [Result].
delete(TableName, PrimaryValue) when is_atom(TableName) ->
    [PrimaryName | _] = db:get_primary_name(TableName),
    DelSql = make_delete_stmt(TableName, PrimaryName, PrimaryValue),
    AffectedRows = db_helper:execute(PrimaryValue, DelSql),
    AffectedRows.

delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    [PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
    DelSql = make_delete_stmt(TableName, PrimaryName1, PrimaryName2, PrimaryValue1, PrimaryValue2),
    AffectedRows = db_helper:execute(PrimaryValue1, DelSql),
    AffectedRows.

delete_all(TableName) ->
    DelSql = lists:concat(["delete from ", TableName]),
    db_helper:execute(DelSql).

make_delete_stmt(TableName, PrimaryName, PrimaryValue) ->
    lists:concat(["delete from ", TableName," where ", PrimaryName, " = ", db:encode(PrimaryValue)]).
make_delete_stmt(TableName, PrimaryName1, PrimaryName2, PrimaryValue1, PrimaryValue2) ->
    lists:concat(["delete from ", TableName,
        " where ", PrimaryName1, " = ", db:encode(PrimaryValue1),
        " and ", PrimaryName2, " = ", db:encode(PrimaryValue2)]).

save(TableName, PrimaryValue, FieldValueList) when
    is_atom(TableName),
    is_list(FieldValueList) ->
    PrimaryNameList = db:get_primary_name(TableName),
    PrimaryList = lists:zip(PrimaryNameList, [PrimaryValue]),
    SaveSql = make_update_and_insert_stmt(TableName, PrimaryList, FieldValueList),
    AffectedRows = db_helper:execute(PrimaryValue, SaveSql),
    AffectedRows.

save(TableName, PrimaryValue1, PrimaryValue2, FieldValueList) when
    is_atom(TableName),
    is_list(FieldValueList) ->
    PrimaryNameList = db:get_primary_name(TableName),
    PrimaryList = lists:zip(PrimaryNameList, [PrimaryValue1, PrimaryValue2]),
    SaveSql = make_update_and_insert_stmt(TableName, PrimaryList, FieldValueList),
    AffectedRows = db_helper:execute(PrimaryValue1, SaveSql),
    AffectedRows.

make_update_and_insert_stmt(TableName, PrimaryList, FieldValueList) when
    is_list(PrimaryList),
    is_list(FieldValueList) ->
    L1 = get_set_value_string(PrimaryList ++ FieldValueList),
    L2 = get_set_value_string(FieldValueList),
    lists:concat(["INSERT into ", TableName," set ", L1, " ON DUPLICATE KEY UPDATE ", L2]);
make_update_and_insert_stmt(TableName, PrimaryName, FieldList) when is_atom(PrimaryName), is_list(FieldList) ->
    L1 = get_set_value_string([PrimaryName | FieldList]),
    L2 = get_set_value_string(FieldList),
    lists:concat(["INSERT into ", TableName," set ", L1, " ON DUPLICATE KEY UPDATE ", L2]).

get_set_value_string(FieldValueList) ->
    Ret = get_set_value_string(FieldValueList, []),
    string:join(Ret, ",").
get_set_value_string([], Ret) ->
    lists:reverse(Ret);
get_set_value_string([{Field, Value} | Rest], Ret) ->
    L = lists:concat([Field, " = ", db:encode(Value)]),
    get_set_value_string(Rest, [L | Ret]).
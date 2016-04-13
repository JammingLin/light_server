%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2015 上午10:40
%%%-------------------------------------------------------------------
-module(db_redis).
-author("jie").

-export([save/2, no_trans_save/2]).
-export([delete/2, delete/3, no_trans_delete/2, no_trans_delete/3]).

-spec save(TableName :: atom(), Record :: map()) -> Result | [Result].
%% 把纪录更新到缓存中, 同时会更落地到数据库中
save(TableName, Record) when is_atom(TableName), is_map(Record) ->
    case trans:is_transaction() of
        true ->
            trans:record_cache_save(?MODULE, TableName, Record);
        false ->
            no_trans_save(TableName, Record)
    end.

no_trans_save(TableName, Record) when is_atom(TableName) ->
    cache:no_trans_save(TableName, Record),
    db:save(TableName, Record).

-spec delete(TableName :: atom(), Record :: map()) -> Result | [Result].
%% 把指定的record从缓存和数据库中删除
delete(TableName, Record) when is_atom(TableName), is_map(Record) ->
    case db:get_primary_name(TableName) of
        [Name] ->
            Value = maps:get(Name, Record),
            delete(TableName, Value);
        [Name1, Name2] ->
            Value1 = maps:get(Name1, Record),
            Value2 = maps:get(Name2, Record),
            delete(TableName, Value1, Value2)
    end;

delete(TableName, PrimaryValue) when is_atom(TableName) ->
    case trans:is_transaction() of
        true ->
            trans:record_cache_delete(?MODULE, TableName, PrimaryValue);
        false ->
            no_trans_delete(TableName, PrimaryValue)
    end.

delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    case trans:is_transaction() of
        true ->
            trans:record_cache_delete(?MODULE, TableName, PrimaryValue1, PrimaryValue2);
        false ->
            no_trans_delete(TableName, PrimaryValue1, PrimaryValue2)
    end.

no_trans_delete(TableName, PrimaryValue) when is_atom(TableName) ->
    cache:no_trans_delete(TableName, PrimaryValue),
    db:delete(TableName, PrimaryValue).

no_trans_delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    cache:no_trans_delete(TableName, PrimaryValue1, PrimaryValue2),
    db:delete(TableName, PrimaryValue1, PrimaryValue2).
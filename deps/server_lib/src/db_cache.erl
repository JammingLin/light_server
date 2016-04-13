%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 八月 2014 9:11
%%%-------------------------------------------------------------------
-module(db_cache).
-author("jie").

-export([get/2, get/3]).
-export([save/2, no_trans_save/2]).
-export([delete/2, delete/3, no_trans_delete/2, no_trans_delete/3]).

-define(DB_CACHE_MODULE, db_redis).

-spec get(TableName :: atom(), PrimaryValue :: any()) -> map() | [map()].
%% 根据表名和主键值，如果缓存中存在数据， 则直接返回， 如果缓存中没有数据， 那么从数据库中装载， 并填充到缓存中
get(TableName, PrimaryValue) when is_atom(TableName) ->
    Result = case cache:no_trans_get(TableName, PrimaryValue) of
        {db_readed, RList} ->
            RList;
        [] ->
            case get_from_db(TableName, PrimaryValue) of
                [] -> [];
                RecordList ->
                    case db:get_primary_name(TableName) of
                        [_] -> hd(RecordList);
                        [_, _] -> RecordList
                    end
            end;
        Record when is_map(Record)->
            Record;
        RecordList when is_list(RecordList) ->
            case db:get(TableName, PrimaryValue) of
                [] ->
                    cache:set_db_readed(TableName, PrimaryValue),
                    [];
                DBRecords ->
                    %% 缓存是最新的, 不能把数据库中获取到的数据覆盖缓存中的数据
                    %% 缓存数据需要和数据库中的数据做合并, 如果有相同的, 以缓存为准
                    [_PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
                    L = [case [R1 || R1 <- RecordList, maps:get(PrimaryName2, R) == maps:get(PrimaryName2, R1)] of
                         [CacheRecord] -> CacheRecord;
                         [] ->
                             cache:no_trans_save(TableName, R),
                             R
                     end || R <- DBRecords],
                    %% 优化, 不能每次都从数据库中读取一遍
                    cache:set_db_readed(TableName, PrimaryValue),
                    L
            end
    end,
    case trans:is_transaction() of
        true -> trans:merge(TableName, PrimaryValue, Result);
        false -> Result
    end.

-spec get(TableName :: atom(), PrimaryValue1 :: any(), PrimaryValue2 :: any()) -> map() | [map()].
get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    Result = case cache:no_trans_get(TableName, PrimaryValue1, PrimaryValue2) of
        {db_readed, RList} ->
            RList;
        [] ->
            Records = db:get(TableName, PrimaryValue1, PrimaryValue2),
            case Records of
                [] ->
                    cache:set_db_readed(TableName, PrimaryValue1, PrimaryValue2),
                    [];
                [Record] ->
                    cache:no_trans_save(TableName, Record),
                    Record;
                _ ->
                    [cache:no_trans_save(TableName, Record) || Record <- Records],
                    Records
            end;
        Record ->
            Record
    end,
    case trans:is_transaction() of
        true -> trans:merge(TableName, PrimaryValue1, PrimaryValue2, Result);
        false -> Result
    end.

-spec save(TableName :: atom(), Record :: map()) -> Result | [Result].
%% 把纪录更新到缓存中, 同时会更落地到数据库中
save(TableName, Record) when is_atom(TableName), is_map(Record) ->
    ok = db_record_check:check(TableName, Record),
    ?DB_CACHE_MODULE:save(TableName, Record).

no_trans_save(TableName, Record) when is_atom(TableName) ->
    ok = db_record_check:check(TableName, Record),
    ?DB_CACHE_MODULE:no_trans_save(TableName, Record).

-spec delete(TableName :: atom(), Record :: map()) -> Result | [Result].
%% 把指定的record从缓存和数据库中删除
delete(TableName, Record) when is_atom(TableName), is_map(Record) ->
    ?DB_CACHE_MODULE:delete(TableName, Record);
delete(TableName, PrimaryValue) when is_atom(TableName) ->
    ?DB_CACHE_MODULE:delete(TableName, PrimaryValue).

delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    ?DB_CACHE_MODULE:delete(TableName, PrimaryValue1, PrimaryValue2).

no_trans_delete(TableName, PrimaryValue) when is_atom(TableName) ->
    ?DB_CACHE_MODULE:no_trans_delete(TableName, PrimaryValue).

no_trans_delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    ?DB_CACHE_MODULE:no_trans_delete(TableName, PrimaryValue1, PrimaryValue2).

%%--------------------------------------------------------------------------------------
get_from_db(TableName, PrimaryValue) ->
    Records = db:get(TableName, PrimaryValue),
    case Records of
        [] ->
            cache:set_db_readed(TableName, PrimaryValue),
            [];
        _ ->
            lists:foreach(fun(Record) -> cache:no_trans_save(TableName, Record) end, Records),
            Records
    end.
%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2015 上午11:06
%%%-------------------------------------------------------------------
-module(cache_ets).
-author("jie").

%% API
-export([start/1, start_link/0, start_link/1]).
-export([no_trans_get/2, no_trans_get/3]).
-export([no_trans_save/2]).
-export([delete_all/0, no_trans_delete/1, no_trans_delete/2, no_trans_delete/3]).
-export([set_db_readed/2]).
-export([make_key/2, make_key/3]).
-export([set/3]).
-export([get_db_keys/0]).
-export([test/0]).

start(_Port) ->
    start_link().

start_link() ->
    Options = [{ets_maxsize, 4*1024*1024*1024}, {ets_threshold, 0.85}, {ets_weight, 30}],
    ets_cache_sup:start_link(Options).

start_link(_Port) ->
    start_link().

%% 进行get操作, 没有事务性
no_trans_get(TableName, PrimaryValue) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue),
    case get_all_impl(Key) of
        db_empty -> db_empty;
        <<"">> -> [];
        {dont_get_from_db, RList} ->
            {dont_get_from_db, get_from_result_list(RList)};
        Record when is_map(Record) ->
            Record;
        Result when is_list(Result)->
            get_from_result_list(Result)
    end.

no_trans_get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue1),
    F = fun(RecordList) ->
        [_PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
        R = list_find(PrimaryName2, PrimaryValue2, RecordList),
        case maps:get(db_empty, R, undefined) of
            true -> db_empty;
            undefined -> R
        end
    end,
    case get_all_impl(Key) of
        db_empty -> db_empty;
        <<"">> -> [];
        {dont_get_from_db, RList} ->
            {dont_get_from_db, F(RList)};
        RecordList when is_list(RecordList) ->
            F(RecordList)
    end.

no_trans_save(TableName, Record) ->
    case db:get_primary_name(TableName) of
        [PrimaryName] ->
            PrimaryValue = maps:get(PrimaryName, Record),
            Key = make_key(TableName, PrimaryValue),
            save_impl(Key, Record);
        [PrimaryName1, PrimaryName2]->
            PrimaryValue1 = maps:get(PrimaryName1, Record),
            PrimaryValue2 = maps:get(PrimaryName2, Record),
            no_trans_save(TableName, PrimaryValue1, PrimaryValue2, Record)
    end.

no_trans_delete(TableName) when is_atom(TableName) ->
    %% todo:ets 还未实现 no_trans_delete/1
    throw({not_implemented_exception, no_trans_delete, 1}).

no_trans_delete(TableName, PrimaryValue) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue),
    cache_server:delete(Key).

no_trans_delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    [_PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
    F = fun(RecordList) ->
        L = lists_delete(PrimaryName2, PrimaryValue2, RecordList),
        Key = make_key(TableName, PrimaryValue1),
        save_impl(Key, L)
    end,
    case no_trans_get(TableName, PrimaryValue1) of
        db_empty -> ok;
        <<"">> -> ok;
        {dont_get_from_db, RList} ->
            {dont_get_from_db, F(RList)};
        RecordList ->
            F(RecordList)
    end.

delete_all() ->
    cache_server:delete_all().

%% %% 保存空的记录到redis中， 避免因为数据库没数据， 导致频繁的穿透到数据库查询
%% set_empty_record(TableName, PrimaryValue) ->
%%     case db:get_primary_name(TableName) of
%%         [_PrimaryName] ->
%%             Key = make_key(TableName, PrimaryValue),
%%             save_impl(Key, db_empty);
%%         [_PrimaryName1, _PrimaryName2] ->
%%             Key = make_key(TableName, PrimaryValue),
%%             save_impl(Key, db_empty)
%%     end.
%%
%% set_empty_record(TableName, PrimaryValue1, PrimaryValue2) ->
%%     [PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
%%     Record1 = maps:put(PrimaryName1, PrimaryValue1, #{}),
%%     Record2 = maps:put(PrimaryName2, PrimaryValue2, Record1),
%%     Record3 = maps:put(db_empty, true, Record2),
%%     no_trans_save(TableName, Record3).

set_db_readed(TableName, PrimaryValue) ->
    RecordList = no_trans_get(TableName, PrimaryValue),
    Term = {dont_get_from_db, RecordList},
    Key = make_key(TableName, PrimaryValue),
    save_impl(Key, Term).

set(Key, Value, TTL) ->
    cache_server:set(Key, Value, TTL).

%% 返回与数据库相关的key 列表
%% 每个key格式如{Table, Key} 或 {Table, Key1, Key2}
get_db_keys() ->
    %% todo:ets 还未实现 get_db_keys
    throw({not_implemented_exception, get_db_keys, 0}).

%%--------------------------------内部函数---------------------------------------
list_find(_PrimaryName2, _PrimaryValue2, []) ->
    false;
list_find(PrimaryName2, PrimaryValue2, [Record | Rest]) ->
    case maps:get(PrimaryName2, Record) of
        PrimaryValue2 -> Record;
        _ ->
            list_find(PrimaryName2, PrimaryValue2, Rest)
    end.

no_trans_save(TableName, PrimaryValue1, PrimaryValue2, Record) when is_atom(TableName)->
    Key = make_key(TableName, PrimaryValue1),
    case no_trans_get(TableName, PrimaryValue1) of
        db_empty -> save_impl(Key, [Record]);
        RecordList when is_list(RecordList)->
            [_PrimaryName1, PrimaryName2] = db:get_primary_name(TableName),
            RecordList2 = lists_delete(PrimaryName2, PrimaryValue2, RecordList),
            RecordList3 = [Record | RecordList2],
            save_impl(Key, RecordList3)
    end.

get_all_impl(Key)->
    cache_server:get(Key).

save_impl(Key, Term) ->
    TTL = 10 * 60, %% 存活时间10分钟
    cache_server:set(Key, Term, TTL).

lists_delete(PrimaryName2, PrimaryValue2, RecordList) ->
    Pred = fun(Record) ->
        case maps:get(PrimaryName2, Record) of
            PrimaryValue2 -> false;
            _ -> true
        end
    end,
    lists:filter(Pred, RecordList).

make_key(TableName, Value) ->
    Str = lists:concat([TableName, ":", Value]),
    list_to_binary(Str).

make_key(TableName, Value1, Value2) ->
    Str = lists:concat([TableName, ":", Value1, ":", Value2]),
    list_to_binary(Str).

get_from_result_list(Result) ->
    Pred = fun(R) ->
        case maps:get(db_empty, R, undefined) of
            true -> false;
            undefined -> true
        end
    end,
    lists:filter(Pred, Result).

%% -----------------------------------test--------------------------------
test() ->
    dgry_server:start('game_db@127.0.0.1'),
    start_link(),
    test_one_primary(),
    test_two_primary(),
%%     test_save_empty_record(),
    io:format("test cache_ets finish!~n").

test_one_primary() ->
    test_one_primary_get(),
    test_one_primary_save(),
    test_one_primary_delete(),
    ok.

test_one_primary_get() ->
    Table = one_primary,
    Id1 = 1,
    [] = cache_ets:no_trans_get(Table, Id1),
    ok.

test_one_primary_save() ->
    Table = one_primary,
    Id1 = 1,
    Record1 = #{id=>Id1, age=>1},
    [] = cache_ets:no_trans_get(Table, Id1),
    cache_ets:no_trans_save(Table, Record1),
    Record1 = cache_ets:no_trans_get(Table, Id1),

    Id2 = 2,
    Record2 = #{id=>Id2, age=>2},
    cache_ets:no_trans_save(Table, Record2),
    Record2 = cache_ets:no_trans_get(Table, Id2),

    Record3 = Record1#{age=>3},
    cache_ets:no_trans_save(Table, Record3),
    Record3 = cache_ets:no_trans_get(Table, Id1),
    ok.

test_one_primary_delete() ->
    cache_ets:delete_all(),
    Table = one_primary,
    Id1 = 1,
    Record1 = #{id=>Id1, age=>1},
    [] = cache_ets:no_trans_get(Table, Id1),
    cache_ets:no_trans_save(Table, Record1),
    Record1 = cache_ets:no_trans_get(Table, Id1),
    cache_ets:no_trans_delete(Table, Id1),
    [] = cache_ets:no_trans_get(Table, Id1),
    ok.

test_two_primary() ->
    test_two_primary_get(),
    test_two_primary_save(),
    test_two_primary_delete(),
    ok.

test_two_primary_get() ->
    Table = two_primary,
    [] = cache_ets:no_trans_get(Table, 1),
    [] = cache_ets:no_trans_get(Table, 1, 1),
    ok.

test_two_primary_save() ->
    cache_ets:delete_all(),

    Table = two_primary,
    PlayerId1 = 1,
    Record1 = #{player_id=>PlayerId1, item_id=>11, age=>111},
    [] = cache_ets:no_trans_get(Table, PlayerId1),
    cache_ets:no_trans_save(Table, Record1),
    [Record1] = cache_ets:no_trans_get(Table, PlayerId1),

    Record11 = Record1#{player_id=>PlayerId1, item_id=>12, age=>112},
    cache_ets:no_trans_save(Table, Record11),
    [Record11, Record1] = cache_ets:no_trans_get(Table, PlayerId1),

    PlayerId2 = 2,
    Record2 = #{player_id=>PlayerId2, item_id=>22, age=>222},
    cache_ets:no_trans_save(Table, Record2),
    [Record2] = cache_ets:no_trans_get(Table, PlayerId2),

    Record3 = Record1#{age=>3},
    cache_ets:no_trans_save(Table, Record3),
    [Record3, Record11] = cache_ets:no_trans_get(Table, PlayerId1),
    ok.

test_two_primary_delete() ->
    cache_ets:delete_all(),
    Table = two_primary,
    PlayerId1 = 1,
    Record1 = #{player_id=>PlayerId1, item_id=>1, age=>1},

    [] = cache_ets:no_trans_get(Table, PlayerId1),
    cache_ets:no_trans_save(Table, Record1),
    [Record1] = cache_ets:no_trans_get(Table, PlayerId1),
    cache_ets:no_trans_delete(Table, PlayerId1),
    [] = cache_ets:no_trans_get(Table, PlayerId1),

    Record2 = #{player_id=>PlayerId1, item_id=>2, age=>2},
    cache_ets:no_trans_save(Table, Record1),
    cache_ets:no_trans_save(Table, Record2),
    [Record2, Record1] = cache_ets:no_trans_get(Table, PlayerId1),
    cache_ets:no_trans_delete(Table, PlayerId1, 1),
    [Record2] = cache_ets:no_trans_get(Table, PlayerId1),
    cache_ets:no_trans_delete(Table, PlayerId1, 2),
    [] = cache_ets:no_trans_get(Table, PlayerId1),

    cache_ets:no_trans_delete(Table, PlayerId1),
    [] = cache_ets:no_trans_get(Table, PlayerId1),
    ok.

%% test_save_empty_record() ->
%%     cache_ets:delete_all(),
%%     Table = one_primary,
%%
%%     cache_ets:set_empty_record(Table, 1),
%%     db_empty = cache_ets:no_trans_get(Table, 1),
%%
%%     Record1 = #{id=>1, age=>1},
%%     cache_ets:no_trans_save(Table, Record1),
%%     Record1 = cache_ets:no_trans_get(Table, 1),
%%
%%     Table2 = two_primary,
%%     cache_ets:set_empty_record(Table2, 1),
%%     db_empty = cache_ets:no_trans_get(Table2, 1),
%%
%%     Table2 = two_primary,
%%     cache_ets:set_empty_record(Table2, 2, 2),
%%     db_empty = cache_ets:no_trans_get(Table2, 2, 2),
%%
%%     Record2 = #{player_id=>1, item_id=>1, age=>1},
%%     cache_ets:no_trans_save(Table2, Record2),
%%     Record2 = cache_ets:no_trans_get(Table2, 1, 1),
%%
%%     Table2 = two_primary,
%%     cache_ets:no_trans_save(Table2, Record2),
%%     Record2 = cache_ets:no_trans_get(Table2, 1, 1),
%%     cache_ets:set_empty_record(Table2, 1, 3),
%%     db_empty = cache_ets:no_trans_get(Table2, 1, 3),
%%     [Record2] = cache_ets:no_trans_get(Table2, 1),
%%
%%     Record3 = Record2#{item_id:=3, age:=3},
%%     cache_ets:no_trans_save(Table2, Record3),
%%     Record3 = cache_ets:no_trans_get(Table2, 1, 3),
%%     [Record3, Record2] = cache_ets:no_trans_get(Table2, 1),
%%     ok.
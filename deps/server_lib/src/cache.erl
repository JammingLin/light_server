%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 八月 2014 10:08
%%%-------------------------------------------------------------------
-module(cache).
-author("Administrator").

-include("cache.hrl").

%% API
-export([start/1, start_link/0, start_link/1]).
-export([get/2, get/3, no_trans_get/2, no_trans_get/3]).
-export([save/2, no_trans_save/2]).
-export([delete/2, delete/3, delete_all/0, no_trans_delete/1, no_trans_delete/2, no_trans_delete/3]).
-export([make_key/2, make_key/3]).
-export([q/1, q/2]).
-export([set_db_readed/2, set_db_readed/3]).
-export([hset/3, hset/4]).
-export([get_db_keys/0]).

start(Port) ->
	Module = get_module(),
	{ok, _} = Module:start(Port).

start_link() ->
    start_link(8001).
start_link(Port) ->
    Module = get_module(),
    Module:start_link(Port).

-spec get(TableName :: atom(), PrimaryValue :: any()) -> map() | [map()].
get(TableName, PrimaryValue) when is_atom(TableName) ->
	Result1 = case no_trans_get(TableName, PrimaryValue) of
		{db_readed, RList} -> RList;
		Result -> Result
	end,
	case trans:is_transaction() of
		true -> trans:merge(TableName, PrimaryValue, Result1);
		false -> Result1
	end.

-spec get(TableName :: atom(), PrimaryValue1 :: any(), PrimaryValue2 :: any()) -> map().
get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
	Result1 = case no_trans_get(TableName, PrimaryValue1, PrimaryValue2) of
		{db_readed, RList} -> RList;
		Result -> Result
	end,
	case trans:is_transaction() of
	true -> trans:merge(TableName, PrimaryValue1, PrimaryValue2, Result1);
	false -> Result1
	end.

%% 对redis进行get操作, 没有事务性
no_trans_get(TableName, PrimaryValue) when is_atom(TableName) ->
	Module = get_module(),
	Module:no_trans_get(TableName, PrimaryValue).

no_trans_get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
	Module = get_module(),
	Module:no_trans_get(TableName, PrimaryValue1, PrimaryValue2).

save(TableName, Record) when is_atom(TableName), is_map(Record) ->
    case trans:is_transaction() of
        true ->
            trans:record_cache_save(?MODULE, TableName, Record);
        false ->
            no_trans_save(TableName, Record)
    end.

no_trans_save(TableName, Record) ->
	Module = get_module(),
	Module:no_trans_save(TableName, Record).

%% 通过主键值, 把数据从缓存和数据库中删除
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

no_trans_delete(TableName) when is_atom(TableName) ->
	Module = get_module(),
	Module:no_trans_delete(TableName).

no_trans_delete(TableName, PrimaryValue1) when is_atom(TableName) ->
	Module = get_module(),
	Module:no_trans_delete(TableName, PrimaryValue1).

no_trans_delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
	Module = get_module(),
	Module:no_trans_delete(TableName, PrimaryValue1, PrimaryValue2).


%% 删除缓存中的所有数据
delete_all() ->
	Module = get_module(),
	Module:delete_all().

%% 设置标记, 已经从数据库中读取过了
set_db_readed(TableName, PrimaryValue) ->
	Module = get_module(),
	Module:set_db_readed(TableName, PrimaryValue).

%% 设置标记, 已经从数据库中读取过了
set_db_readed(TableName, PrimaryValue1, PrimaryValue2) ->
	Module = get_module(),
	Module:set_db_readed(TableName, PrimaryValue1, PrimaryValue2).

%% set和save的区别是,set不依赖数据库中的表的结构信息, 而save是依赖的, 所以一般save和db是关联的
hset(Key, _Field, Value) ->
	hset(Key, Value, infinity).

hset(Key, _Field, Value, TTL) ->
	Module = get_module(),
	Module:set(Key, Value, TTL).

%% 返回与数据库相关的key 列表
%% 每个key格式如{Table, Key} 或 {Table, Key1, Key2}
get_db_keys() ->
    Module = get_module(),
    Module:get_db_keys().

q(Command) ->
    q(Command, ?TIMEOUT).

q(Command, Timeout) ->
    Module = get_module(),
    Module:q(Command, Timeout).

make_key(TableName, Value) ->
	Module = get_module(),
	Module:make_key(TableName, Value).

make_key(TableName, Value1, Value2) ->
	Module = get_module(),
	Module:make_key(TableName, Value1, Value2).

get_module() ->
    case application:get_env(game, cache_record) of
        {ok, Module} -> Module;
        _ -> cache_redis
    end.
%% 	cache_ets.
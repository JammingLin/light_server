%%%-------------------------------------------------------------------
%%% @author linyijie
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2014 9:47
%%%-------------------------------------------------------------------
-module(cache_redis).
-author("Administrator").

-include("cache.hrl").

-define(SERVER, eredis).
-define(EXPIRE_TIME, 60 * 60). %% 15分钟后, 缓存可能被回收

%% API
-export([start/1, start_link/0, start_link/1]).
-export([no_trans_get/2, no_trans_get/3]).
-export([no_trans_save/2]).
-export([delete_all/0, no_trans_delete/1, no_trans_delete/2, no_trans_delete/3]).
-export([q/1, q/2]).
-export([set_db_readed/2, set_db_readed/3]).
-export([get_db_keys/0]).

start(Port) ->
    Pid = ensure_started(Port),
    {ok, Pid}.

start_link() ->
    start_link(8001).

start_link(Port) when is_integer(Port) ->
    Pid = ensure_started(Port),
    {ok, Pid}.

%% 对redis进行get操作, 没有事务性
no_trans_get(TableName, PrimaryValue) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue),
    case db:get_primary_name(TableName) of
        [_PrimaryName] ->
            case q([get, Key]) of
                {ok, undefined} -> [];
                {ok, <<"db_readed">>} -> {db_readed, []};
                {ok, Binary} -> binary_to_term(Binary)
            end;
        [_PrimaryName1, _PrimaryName2] ->
            case q([hgetall, Key]) of
                {ok, []} -> [];
                {ok, KVList} ->
                    case get_record_list(KVList, {undefined, []}) of
                        {undefined, Records} -> Records;
                        {db_readed, _} = Readed -> Readed
                    end
            end
    end.

no_trans_get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue1),
    case q([hget, Key, PrimaryValue2]) of
        {ok, undefined} -> [];
        {ok, <<"db_readed">>} -> {db_readed, []};
        {ok, Binary} -> binary_to_term(Binary)
    end.


no_trans_save(TableName, Record) ->
    case db:get_primary_name(TableName) of
        [PrimaryName] ->
            PrimaryValue = maps:get(PrimaryName, Record),
            Key = make_key(TableName, PrimaryValue),
            Term = term_to_binary(Record),
            case q([set, Key, Term, ex, ?EXPIRE_TIME]) of
                {ok, _} -> ok;
                Error -> throw({error, {Error, Record}})
            end;
        [PrimaryName1, PrimaryName2] ->
            PrimaryValue1 = maps:get(PrimaryName1, Record),
            PrimaryValue2 = maps:get(PrimaryName2, Record),
            Key = make_key(TableName, PrimaryValue1),
            Term = term_to_binary(Record),
            hset(Key, PrimaryValue2, Term)
    end.

no_trans_delete(TableName) when is_atom(TableName) ->
    {ok, Keys} = q([keys, lists:concat([TableName, ":*"])]),
    LKeys = utils:splitlist(Keys, 5000),

    [begin
         MultiKey = utils:joinlist(SubKeys, " "),
         delete_impl(MultiKey)
     end || SubKeys <- LKeys].

no_trans_delete(TableName, PrimaryValue1) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue1),
    delete_impl(Key).

no_trans_delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    Key = make_key(TableName, PrimaryValue1),
    {ok, Count} = q([hdel, Key, PrimaryValue2]),
    binary_to_integer(Count).

%% 删除缓存中的所有数据
delete_all() ->
    q([flushall]).

%%-----------------------------------------内部函数-----------------------------------------
hset(Key, Field, Value) ->
    {ok, _} = q([hset, Key, Field, Value]),
    {ok, _} = q([expire, Key, ?EXPIRE_TIME]).

get_record_list([], Acc) ->
    Acc;
get_record_list([<<"*read_flag*">>, _ReadFlag | Rest], {_, Records}) ->
    get_record_list(Rest, {db_readed, Records});
get_record_list([_, <<"db_readed">> | Rest], {ReadFlag, Records}) ->
    get_record_list(Rest, {ReadFlag, Records});
get_record_list([_, Bin | Rest], {ReadFlag, Records}) ->
    get_record_list(Rest, {ReadFlag, [binary_to_term(Bin) | Records]}).

delete_impl(Key) ->
    {ok, Count} = q([del, Key]),
    binary_to_integer(Count).

set_db_readed(TableName, PrimaryValue) ->
    Key = make_key(TableName, PrimaryValue),
    case db:get_primary_name(TableName) of
        [_PrimaryName] ->
            q([set, Key, db_readed, ex, ?EXPIRE_TIME]);
        [_PrimaryName1, _PrimaryName2] ->
            hset(Key, "*read_flag*", db_readed)
    end.

set_db_readed(TableName, PrimaryValue1, PrimaryValue2) ->
    Key = make_key(TableName, PrimaryValue1),
    {ok, _} = hset(Key, PrimaryValue2, db_readed).

%% 返回与数据库相关的key 列表
%% 每个key格式如{Table, Key} 或 {Table, Key1, Key2}
get_db_keys() ->
    {ok, Keys} = q([keys, "*"]),
    Keys1 = [begin
                 Sagments = utils:split(Key, $:),
                 case Sagments of
                     [] ->
                         skip;
                     [_Single] ->
                         skip;
                 %% todo: 还要跳过 _list 结尾的 Table
                     [Table, PrimaryKey] ->
                         {Table, PrimaryKey};
                     [Table, PrimaryKey1, PrimaryKey2] ->
                         {Table, PrimaryKey1, PrimaryKey2}
                 end
             end || Key <- Keys],
    [K || K <- Keys1, K =/= skip].

make_key(TableName, Value) ->
    lists:concat([TableName, ":", translate_value(Value)]).

translate_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
translate_value(Value) ->
    Value.

q(Command) ->
    q(Command, ?TIMEOUT).

q(Command, Timeout) ->
    ensure_started(8001),
    eredis:q(eredis, Command, Timeout).

ensure_started(Port) ->
    case erlang:whereis(?SERVER) of
        undefined ->
            Props = application:get_env(game, redis, []),
            Host = proplists:get_value(host, Props, "127.0.0.1"),
            Port1 = proplists:get_value(port, Props, Port),
            Database = proplists:get_value(database, Props, 0),
            Password = proplists:get_value(password, Props, ""),
            ReconnectSleep = proplists:get_value(reconnect_sleep, Props, 1000),
            ConnectTimeout = proplists:get_value(connect_timeout, Props, ?TIMEOUT),
            {ok, Pid} = gen_server:start_link({local, ?SERVER}, eredis_client, [Host, Port1, Database, Password,
                ReconnectSleep, ConnectTimeout], []),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:57
%%%-------------------------------------------------------------------
-module(db_mysql_common).
-author("long").

%% API
-export([start/0, start/1, stop/0]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([to_map/1]).

-define(DB_POOLNAME, mysql).
-define(DB_EXECUTE_TIMEOUT,  5 * 1000).  %% 每次数据库操作的超时时间

start() ->
    {ok, [Config | _]} = application:get_env(db, db_pools),
    start(Config).

start({_PoolName, Props}) ->
    start_one(?DB_POOLNAME, Props);

start(Pools) when is_list(Pools) ->
    [start_one(PoolName, Props) || {PoolName, Props}  <- Pools].


start_one(PoolName, Props) ->
    PoolSize = proplists:get_value(size, Props),
    Host = proplists:get_value(host, Props),
    User = proplists:get_value(user, Props),
    Pwd = proplists:get_value(password, Props),
    DataBase = proplists:get_value(database, Props),
    Port = proplists:get_value(port, Props),
    Encoding = proplists:get_value(encoding, Props, utf8),

    case mysql:start_link(PoolName, Host, Port, User, Pwd, DataBase, fun log/4, Encoding) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    lists:foreach(fun(_) ->
        case mysql:connect(PoolName, Host, Port, User, Pwd, DataBase, Encoding, true) of
            {ok, _} -> ok;
            {error, _Reason}  = Err ->
                lager:error("connect_db_failed, PoolName:~p, Host:~p, Port:~p, User:~p, Pwd:~p, DataBase:~p, Reason:~p",
                    [PoolName, Host, Port, User, Pwd, DataBase, _Reason]),
                throw(Err)
        end
    end, lists:seq(1, PoolSize)),
    PoolName.

stop() ->
    ok.

log(_Module, _Line, _Level, _FormatFun) ->
    ok.

query(Query) ->
    {data, Result} = mysql:fetch(?DB_POOLNAME, Query, ?DB_EXECUTE_TIMEOUT),
    Records = to_map(Result),
    Records.

query(PoolName, Query) ->
    {data, Result} = mysql:fetch(PoolName, Query, ?DB_EXECUTE_TIMEOUT),
    Records = to_map(Result),
    Records.

execute(Query) ->
    execute(?DB_POOLNAME, Query).

execute(PoolName, Query) ->
    case execute2(PoolName, Query) of
        {data, Result} ->
            mysql:get_result_rows(Result);
        {updated, Result} ->
            mysql:get_result_affected_rows(Result);
        Error ->
            throw({error, {Query, Error}})
    end.

execute2(Query) ->
    execute2(?DB_POOLNAME, Query).

execute2(PoolName, Query) ->
    Result = mysql:fetch(PoolName, Query, ?DB_EXECUTE_TIMEOUT),
    case Result of
        {data, _} -> Result;
        {updated, _} -> Result;
        Error ->
            throw({error, {Query, Error}})
    end.

to_map({data, Result}) ->
    to_map(Result);
to_map(Result) ->
    FieldList = mysql:get_result_field_info(Result),
    Rows = mysql:get_result_rows(Result),
    KVList = get_kv_list(FieldList, Rows),
    Records = [maps:from_list(L) || L <- KVList],
    Records.

get_kv_list(FieldList, ValueList) ->
    [get_kv_list(FieldList, Record, []) || Record <- ValueList].

get_kv_list([], [], KVList) ->
    KVList;
get_kv_list([{_, Name, _, _}|FieldRest], [Value | ValueRest], KVList) ->
    Name1 = erlang:binary_to_atom(Name, utf8),
    get_kv_list(FieldRest, ValueRest, [{Name1, translate:from_mysql(Value)} | KVList]).
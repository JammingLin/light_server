%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 八月 2014 9:52
%%%-------------------------------------------------------------------
-module(db_helper).
-author("Administrator").

-behaviour(gen_server).

-include_lib("emysql.hrl").

%% API
-export([start_link/1, load_mysql_schema/0, copy_db_schema/0, get_db_module/0]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([get/2, get/3, save/2, delete/2, delete/3, delete_all/1]).
-export([get_tables/0]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(DB_MODULE, db_real).  %% db_real or db_timer
-define(DEFAULT_MYSQL_MODULE, db_mysql). %% db_mysql, db_mysql_async, db_mysql_cluster
-define(SERVER, ?MODULE).
-define(UPDATE_TIMEOUT, 5 * 3600 * 1000).
-record(state, {}).

start_link(SqlPath) ->
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [SqlPath], [{timeout, ?UPDATE_TIMEOUT}]).

get_db_module() ->
    ?DB_MODULE.

copy_db_schema() ->
    %% 这边设置的超时时间最多为5个小时， 也就是说更新最多只能在5个小时之内完成。
    gen_server:call(?SERVER, copy_db_schema, ?UPDATE_TIMEOUT).

init([SqlPath]) ->
    process_flag(trap_exit, true),
    Module = get_module(),
    Module:start(SqlPath),
    ets:new(mysql_schema, [public,set,named_table]),
    load_mysql_schema(),
    case Module of
        db_mysql_cluster -> db_mysql_cluster:move();
        _ -> ok
    end,
    {ok, #state{}}.

handle_call(copy_db_schema, _From, State) ->
    {reply, ets:tab2list(mysql_schema), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    Module = get_module(),
    Module:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 载入mysql的shema数据， 主要载入数据库对应的所有表名和主键名
load_mysql_schema() ->
    Tables = get_tables(),
    ets:delete_all_objects(mysql_schema),
    [load_mysql_schema(Table) || Table <- Tables].

load_mysql_schema(Table) ->
    F = fun({PrimaryNameList, Columns}) ->
        #{primary_name_list => PrimaryNameList,columns => Columns}
    end,
    TableInfo = get_columns_info(Table),
    ets:insert(mysql_schema, {Table, F(TableInfo)}).

get_tables() ->
    DBName = get_db_name(),
    Query = lists:concat(["SELECT table_name FROM information_schema.tables WHERE table_schema = '", DBName, "'"]),
    Records = query(Query),
    [binary_to_atom(TableName, utf8) || #{table_name := TableName} <- Records].

get_db_name() ->
    {ok, [{_PoolName, Config} |_]} = application:get_env(db, db_pools),
    proplists:get_value(database, Config).

get_columns_info(Table) ->
    Query = lists:concat(["SHOW COLUMNS FROM ", Table]),
    Result = query(Query),
    PrimaryNames = case [Name || #{'Key':= <<"PRI">>, 'Field':=Name} <- Result] of
                    [] -> throw({error, lists:concat(["not found table primary key,  table:", Table])});
                    Names -> [binary_to_atom(Name, utf8) || Name <- Names]
                  end,
    {PrimaryNames,
        [{erlang:binary_to_atom(Name, utf8), translate:translate_type(Type)} ||
            #{'Type':= Type, 'Field':=Name}<- Result]}.

get(TableName, PrimaryValue) ->
    ?DB_MODULE:get(TableName, PrimaryValue).

get(TableName, PrimaryValue1, PrimaryValue2) ->
    ?DB_MODULE:get(TableName, PrimaryValue1, PrimaryValue2).

save(TableName, Record) when is_map(Record)->
    ?DB_MODULE:save(TableName, Record).

delete(TableName, PrimaryValue) ->
    ?DB_MODULE:delete(TableName, PrimaryValue).

delete(TableName, PrimaryValue1, PrimaryValue2) ->
    ?DB_MODULE:delete(TableName, PrimaryValue1, PrimaryValue2).

delete_all(TableName) ->
    ?DB_MODULE:delete_all(TableName).

%% 执行insert, delete, update等操作, 返回受到影响的行数
execute(Query) ->
    Module = get_module(),
    Module:execute(Query).

%% 执行insert, delete, update等操作, 返回受到影响的行数
execute(Key, Query) ->
    Module = get_module(),
    Module:execute(Key, Query).

%% 执行insert, delete, update等操作, 返回ok_packet
execute2(Query) ->
    Module = get_module(),
    Module:execute2(Query).

%% 执行insert, delete, update等操作, 返回ok_packet
execute2(Key, Query) ->
    Module = get_module(),
    Module:execute2(Key, Query).

query(Query) ->
    Module = get_module(),
    Module:query(Query).

query(Key, Query) ->
    Module = get_module(),
    Module:query(Key, Query).

get_module() ->
    case application:get_env(db, db_module) of
        {ok, Module} -> Module;
        _ -> ?DEFAULT_MYSQL_MODULE
    end.

%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 一月 2015 15:43
%%%-------------------------------------------------------------------
-module(db_proxy).
-author("long").

-include_lib("emysql.hrl").
%% API
-export([start_link/1]).
-export([get/2, get/3]).
-export([save/2]).
-export([delete/2, delete/3, delete_all/1]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([copy_mysql_schema/1, get_table_schema/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(DBNode) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBNode], []).

init([_DBNode]) ->
%%     DBNode1 = get_db_node(DBNode),
%%     connect_db(DBNode1, infinity),
%%     lager:info("connected db"),
%%     copy_mysql_schema(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% connect_db(_DBNode, 0) ->
%%     connect_error;
%% connect_db(DBNode, ReconnectCount) ->
%%     lager:info("connecting db:~p", [DBNode]),
%%     case net_adm:ping(DBNode) of
%%         pong -> ok;
%%         pang ->
%%             timer:sleep(1000),
%%             case ReconnectCount == infinity of
%%                 true -> connect_db(DBNode, ReconnectCount);
%%                 false -> connect_db(DBNode, ReconnectCount-1)
%%             end
%%     end.
%%
%% get_db_node([]) ->
%%     case init:get_argument(db_server) of
%%         error -> error;
%%         {badrpc,_} -> error;
%%         {ok, [[Node]]} ->
%%             Node1 = list_to_atom(Node),
%%             application:set_env(game, db_node, Node1),
%%             Node1
%%     end;
%% get_db_node(DBNode) ->
%%     application:set_env(game, db_node, DBNode),
%%     DBNode.
%%
%% get_db_node() ->
%%     application:get_env(game, db_node, node()).

%% %% 载入mysql的shema数据， 主要载入数据库对应的所有表名和主键名
%% copy_mysql_schema() ->
%%     Node = get_db_node(),
%%     copy_mysql_schema(Node).

copy_mysql_schema(Node) ->
    %% 如果表已经存在, 则不再创建, 这种情况出现在db节点和game节点在同一个节点中
    case ets:info(mysql_schema) of
        undefined ->
            ets:new(mysql_schema, [public,set,named_table]);
        _ -> ok
    end,
    copy_mysql_schema_data(Node).

copy_mysql_schema_data(Node) ->
    case rpc:call(Node, db_helper, copy_db_schema, [], 5 * 1000) of
        {badrpc, _} ->
            timer:sleep(1000),
            copy_mysql_schema_data(Node);
        Schema ->
            ets:insert(mysql_schema, Schema)
    end.

get_table_schema(Table) ->
    [{_Tab, #{columns:=Columns}}] = ets:lookup(mysql_schema, Table),
    Columns.


rpc(M, F, A) ->
    apply(M, F, A).

cast(M, F, A) ->
    apply(M, F, A).

get(TableName, PrimaryValue) ->
    rpc(db_helper, get, [TableName, PrimaryValue]).

get(TableName, PrimaryValue1, PrimaryValue2) ->
    rpc(db_helper, get, [TableName, PrimaryValue1, PrimaryValue2]).

save(TableName, Record) ->
    cast(db_helper, save, [TableName, Record]).

delete(TableName, PrimaryValue) ->
    cast(db_helper, delete, [TableName, PrimaryValue]).

delete(TableName, PrimaryValue1, PrimaryValue2) ->
    cast(db_helper, delete, [TableName, PrimaryValue1, PrimaryValue2]).

delete_all(TableName) ->
    cast(db_helper, delete_all, [TableName]).

query(Query) ->
    rpc(db_helper, query, [Query]).

query(Key, Query) ->
    rpc(db_helper, query, [Key, Query]).

execute(Query) ->
    rpc(db_helper, execute, [Query]).

execute(Key, Query) ->
    rpc(db_helper, execute, [Key, Query]).

execute2(Query) ->
    rpc(db_helper, execute2, [Query]).

execute2(Key, Query) ->
    rpc(db_helper, execute2, [Key, Query]).

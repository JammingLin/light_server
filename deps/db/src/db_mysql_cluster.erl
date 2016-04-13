%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:57
%%%-------------------------------------------------------------------
-module(db_mysql_cluster).
-author("long").

%% API
-export([start/1, stop/0, move/0, move/1, check_move/0]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([to_map/1]).

-define(NORMAL_RING, db_ring).

start(SqlPath) ->
    {ok, Pool} = application:get_env(db, db_pools),
    setup_ring(Pool),
%%     db_mysql_async:start(Pool, SqlPath).
    db_mysql_common:start(Pool),
    db_update:start(Pool, SqlPath).

setup_ring(Pool) ->
    {NodeNameList, _} = lists:unzip(Pool),
    consist_hash:store_ring(?NORMAL_RING, NodeNameList).

stop() ->
    db_mysql_common:stop().

%% 数据迁移, 用于增加新的数据库节点的时候, 对数据分布存储, 降低单一数据库压力
move() ->
    lager:info("mysql cluster, moving data..."),
    {ok, CurrPool} = application:get_env(db, db_pools),
    move(CurrPool),
    lager:info("move data finish!!!").

move(CurrPool) ->
    Query = "select * from cluster_nodes where id = 1",
    OldPool = case db_mysql_common:query(Query) of
                  [] -> [lists:keyfind(mysql, 1, CurrPool)]; %% 默认就有一个mysql节点了
                  [#{id :=1, nodes:=OldPool0}] -> utils:string_to_data(OldPool0)
              end,

    setup_ring(CurrPool),

    {DelNodes, AddNodes} = get_change_nodes(OldPool, CurrPool),
    Pools = get_all_nodes(OldPool, CurrPool),
    add_node_move(AddNodes, Pools),
    del_node_move(DelNodes, Pools),

    Update = lists:concat(["replace into cluster_nodes set id = 1, nodes = ",
        db:encode(utils:term_to_string(CurrPool))]),
    {updated, _} = db_mysql_common:execute2(Update).


get_change_nodes(OldNodes, CurrNodes) ->
    {OldNodes1, _} = lists:unzip(OldNodes),
    {CurrNodes1, _} = lists:unzip(CurrNodes),
    DelNodes = OldNodes1 -- CurrNodes1,
    AddNodes = CurrNodes1 -- OldNodes1,
    {DelNodes, AddNodes}.

get_all_nodes(OldPools, NewPools) ->
    OldPools1 = lists:sort(OldPools),
    NewPools1 = lists:sort(NewPools),
    lists:umerge(OldPools1, NewPools1).

add_node_move(NewNodes, Pool) ->
    %% 做数据移动的时候需要让gate不能对外服务, 否则玩家这个时候可能登录进行数据库操作
    consist_hash:add_node_update(?NORMAL_RING, NewNodes, fun(NewRing, OldNode) ->
        SameNodeFun = fun(OldNode1, NewNode) ->
            {_Name1, Config1} = lists:keyfind(OldNode1, 1, Pool),
            Host1 = proplists:get_value(host, Config1),
            Database1 = proplists:get_value(database, Config1),

            {_Name2, Config2} = lists:keyfind(NewNode, 1, Pool),
            Host2 = proplists:get_value(host, Config2),
            Database2 = proplists:get_value(database, Config2),

            Host1 == Host2 andalso Database1 == Database2
        end,

        Tables0 = db_helper:get_tables(),
        Tables1 = lists:delete(update_history, Tables0),
        Tables = lists:delete(cluster_nodes, Tables1),

        lists:foreach(fun(TableName) ->
            [PrimaryName | _] = db:get_primary_name(TableName),
            move_impl(TableName, PrimaryName, OldNode, NewRing, SameNodeFun, [], 100)
        end, Tables)
    end).

del_node_move(DelNodes, Pool) ->
    %% 做数据移动的时候需要让gate不能对外服务, 否则玩家这个时候可能登录进行数据库操作
    consist_hash:del_node_update(?NORMAL_RING, DelNodes, fun(NewRing, DelNode) ->
        SameNodeFun = fun(DelNode1, NewNode) ->
            {_Name1, Config1} = lists:keyfind(DelNode1, 1, Pool),
            Host1 = proplists:get_value(host, Config1),
            Database1 = proplists:get_value(database, Config1),

            {_Name2, Config2} = lists:keyfind(NewNode, 1, Pool),
            Host2 = proplists:get_value(host, Config2),
            Database2 = proplists:get_value(database, Config2),

            Host1 == Host2 andalso Database1 == Database2
        end,

        Tables0 = db_helper:get_tables(),
        Tables1 = lists:delete(update_history, Tables0),
        Tables = lists:delete(cluster_nodes, Tables1),

        lists:foreach(fun(TableName) ->
            [PrimaryName | _] = db:get_primary_name(TableName),
            move_impl(TableName, PrimaryName, DelNode, NewRing, SameNodeFun, [], 100)
        end, Tables)
    end).

move_impl(TableName, PrimaryName, MoveNode, NewRing, SameNodeFun, LastData, Count) ->
    Rows = get_data(TableName, LastData, Count, MoveNode),
    [begin
         Key = maps:get(PrimaryName, Row),
         case consist_hash:can_move(NewRing, MoveNode, Key, SameNodeFun) of
             {true, _NewNode} ->
                 %% add
                 db:save(TableName, Row),

                 %% delete:
                 Del = db:make_delete_stmt(TableName, Row),
                 1 = db_mysql_common:execute(MoveNode, Del),
                 ok;
             false -> ok
         end
     end || Row <- Rows],
    Len = length(Rows),
    case Len == Count of
        true -> move_impl(TableName, PrimaryName, MoveNode, NewRing, SameNodeFun, lists:last(Rows), Count);
        false -> ok
    end.

get_data(TableName, LastData, Count, OldNode) ->
    Query = case LastData of
        [] -> lists:concat(["select * from ", TableName, " limit 0, ", Count]);
        _ ->
            [PrimaryName | _ ] = db:get_primary_name(TableName),
            Val = db:encode(maps:get(PrimaryName, LastData)),
            lists:concat(["select * from ", TableName, " where ", PrimaryName, " >= ", Val, " limit ", Count])
    end,
    Rows = db_mysql_common:query(OldNode, Query),
    Rows.

%% 检查数据迁移后是否正确
check_move() ->
    Tables0 = db_helper:get_tables(),
    Tables = lists:delete(update_history, Tables0),

    {ok, Pool} = application:get_env(db, db_pools),
    {Nodes, _} = lists:unzip(Pool),

    lists:foreach(fun(TableName) ->
        [check_move_impl(TableName, [], 100, Node) || Node <- Nodes]
    end, Tables).

check_move_impl(TableName, LastData, Count, Node) ->
    Rows = get_data(TableName, LastData, Count, Node),
    [begin
         case db:get_primary_name(TableName) of
            [PrimaryName] ->
                Val = maps:get(PrimaryName, Row),
                Query = lists:concat(["select * from ", TableName, " where ", PrimaryName, " = ", db:encode(Val)]),
                Node1 = consist_hash:lookup(?NORMAL_RING, Val),
                case length(db_mysql_common:execute(Node1, Query)) of
                    1 -> ok;
                    Count1 -> throw({error, Query, Node1, Count1})
                end;
            [PrimaryName1, PrimaryName2] ->
                Val1 = maps:get(PrimaryName1, Row),
                Val2 = maps:get(PrimaryName2, Row),
                Query = lists:concat(["select * from ", TableName, " where ",
                    PrimaryName1, " = ", db:encode(Val1), " and ",
                    PrimaryName2, " = ", db:encode(Val2)]),
                Node1 = consist_hash:lookup(?NORMAL_RING, Val1),
                case length(db_mysql_common:execute(Node1, Query)) of
                    1 -> ok;
                    Count1 -> throw({error, Query, Node1, Count1})
                end
         end
     end || Row <- Rows],
    case length(Rows) == Count of
        true -> check_move_impl(TableName, lists:last(Rows), Count, Node);
        false -> ok
    end.


query(Query) ->
    db_mysql_common:query(Query).

query(Key, Query) ->
    PoolName = get_pool_name(Key),
    db_mysql_common:query(PoolName, Query).

execute(Query) ->
    db_mysql_common:execute(Query).

execute(Key, Query) ->
    PoolName = get_pool_name(Key),
    db_mysql_common:execute(PoolName, Query).

execute2(Query) ->
    db_mysql_common:execute2(Query).

execute2(Key, Query) ->
    PoolName = get_pool_name(Key),
    db_mysql_common:execute2(PoolName, Query).

to_map(Result) ->
    db_mysql_common:to_map(Result).

get_pool_name(Key) ->
    PoolName = consist_hash:lookup(?NORMAL_RING, Key),
    PoolName.
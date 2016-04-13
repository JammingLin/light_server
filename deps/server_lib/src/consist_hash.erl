%%%-------------------------------------------------------------------
%%% 分布式集群需要用到的一致性hash算法
%%%-------------------------------------------------------------------

-module(consist_hash).

-export([lookup/2, create_ring/1, store_ring/2, add_node_update/3, del_node_update/3, can_move/4, get_prev_node/2]).
-export([test/0]).
%% @doc The consistent hash ring spans from 0 to 2^160 - 1.
-define(RINGUPPER, trunc(math:pow(2,160) - 1)).

-type nodename() :: term().
-type nodeconfig() :: term().
-type cnode() :: {nodename(), nodeconfig()}.
-type cring() :: [{integer(), cnode()}].


-spec lookup(term(), term()) -> cnode().
lookup(Name, Key) ->
    {ok, Ring} = application:get_env(?MODULE, Name),
    lookup_impl(Ring, Key).

lookup_impl(Ring, Key) ->
    Hash = hash(Key),
    {{_, Node}, {_, Max}} =
        lists:foldl(fun({C, CNode}, {{L,LNode}, {U,UNode}}) ->
                            case C =< Hash of
                                true ->
                                    case C > L of
                                        true ->
                                            {{C, CNode}, {U, UNode}};
                                        false ->
                                            {{L, LNode}, {U, UNode}}
                                    end;
                                false ->
                                    case C > U of
                                        true ->
                                            {{L, LNode}, {C, CNode}};
                                        false ->
                                            {{L, LNode}, {U, UNode}}
                                    end
                            end
                    end,
                    {{-1, undefined}, {-1, undefined}},
                    Ring),
    case Node of
        undefined ->
            Max;
        _ ->
            Node
    end.

-spec store_ring(term(), [term()]) ->cring().
store_ring(Name, Nodes) ->
    Ring = create_ring(Nodes),
    application:set_env(?MODULE, Name, Ring),
    Ring.

-spec create_ring([term()]) -> cring().
create_ring(Nodes) ->
%%     NumShards = length(Nodes),
%%     Interval = ?RINGUPPER div NumShards,
%%     Points = [ X * Interval || X <- lists:seq(0, NumShards-1) ],
%%     lists:zip(Points, Nodes).
    [{hash(Node), Node} || Node <- Nodes].

-spec add_node_update(Name, NewNodes, MoveFun) -> ok when
    Name :: term(),
    NewNodes :: [term()],
    MoveFun :: fun((Ring :: cring(), OldNode :: term()) -> ok).
add_node_update(Name, NewNodes, MoveFun) when is_list(NewNodes)->
    {ok, Ring} = application:get_env(?MODULE, Name),
    add_node_update_impl(Ring, NewNodes, MoveFun);
add_node_update(Name, NewNode, MoveFun) ->
    add_node_update(Name, [NewNode], MoveFun).

del_node_update(Name, DelNodes, MoveFun) when is_list(DelNodes)->
    {ok, Ring} = application:get_env(?MODULE, Name),
    del_node_update_impl(Ring, DelNodes, MoveFun);
del_node_update(Name, NewNode, MoveFun) ->
    del_node_update(Name, [NewNode], MoveFun).

add_node_update_impl(Ring, NewNodes, MoveFun) ->
    F = fun(NewNode) ->
        OldNode = get_prev_node(NewNode, Ring),
        MoveFun(Ring, OldNode)
    end,
    lists:foreach(F, NewNodes).

del_node_update_impl(Ring, DelNodes, MoveFun) ->
    F = fun(DelNode) ->
        MoveFun(Ring, DelNode)
    end,
    lists:foreach(F, DelNodes).

can_move(Ring, OldNode, Key, SameNodeFun) ->
    NewNode = lookup_impl(Ring, Key),
    case OldNode == NewNode of
        true -> false;
        false ->
            %% 本算法支持虚拟节点, 即多个虚拟节点可以映射到同一个物理节点
            %% 比如多个虚拟节点对应同一个数据库, 这样的好处是数据分布会更均匀
            case SameNodeFun(OldNode, NewNode) of
                true -> false;
                false -> {true, NewNode}
            end
    end.

%% add_ring([], Ring)->
%%     Ring;
%% add_ring([Node | Rest], Ring)->
%%     Ring1 = add_ring(Node, Ring),
%%     add_ring(Rest, Ring1);
%% add_ring(Node, Ring) ->
%%     Ring1 = [{hash(Node), Node} | Ring],
%%     lists:keysort(1, Ring1).

get_new_nodes(Nodes, Ring) ->
    get_new_nodes(Nodes, Ring, []).

get_new_nodes([], _Ring, NewNodes) ->
    NewNodes;
get_new_nodes([{NodeName, _Config} | Rest], Ring, NewNodes) ->
    case lists:keymember(NodeName, 2, Ring) of
        true -> get_new_nodes(Rest, Ring, NewNodes);
        false -> get_new_nodes(Rest, Ring, [NodeName | NewNodes])
    end.

get_prev_node(NodeName, Ring) ->
    Index = get_index(NodeName, Ring),
    {_, PrevNodeName} = case Index of
        1 -> lists:last(Ring);
        _ -> lists:nth(Index - 1, Ring)
    end,
    PrevNodeName.

get_index(NodeName, Ring) ->
    get_index(NodeName, Ring, 1).

get_index(NodeName, [{_, NodeName} | _Rest], Index) ->
    Index;
get_index(NodeName, [{_, _} | Rest], Index) ->
    get_index(NodeName, Rest, Index+1).


-spec hash(binary() | list()) -> integer().
hash(Key) when is_binary(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, Key),
    Hash;
hash(Key) when is_atom(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, list_to_binary(atom_to_list(Key))),
    Hash;
hash(Key) when is_list(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, list_to_binary(Key)),
    Hash;
hash(Key) when is_integer(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, integer_to_binary(Key)),
    Hash;
hash(Key) when is_float(Key) ->
    <<Hash:160/integer>> = crypto:hash(sha, float_to_binary(Key)),
    Hash.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
%%     test_add_ring(),
    test_get_new_nodes(),
    test_get_prev_node(),
    test_move(),
    test_nodes_same_config(),
    test_move2(),
    io:format("consist hash finish~n").

%% test_add_ring() ->
%%     Ring = create_ring([a, b, c]),
%%     [{0,a},
%%     {343748583428966666016543133416491682410858883188, d},
%%     {487167212443634306067894944238761006551977514325, b},
%%     {974334424887268612135789888477522013103955028650, c}] = add_ring(d, Ring),
%%
%%     [{0,a},
%%     {343748583428966666016543133416491682410858883188,d},
%%     {487167212443634306067894944238761006551977514325,b},
%%     {507536008161074956020911211658791321121274335871,e},
%%     {974334424887268612135789888477522013103955028650,c}] = add_ring([d, e], Ring),
%%     ok.

test_get_new_nodes() ->
    Ring = create_ring([a, b, c]),
    [e, d] = get_new_nodes([{a, a_config}, {b, b_config}, {c, c_config}, {d, d_config}, {e, e_config}], Ring),
    ok.

test_get_prev_node() ->
    Ring = create_ring([a, b, c]),
    c = get_prev_node(a, Ring),
    a = get_prev_node(b, Ring),
    b = get_prev_node(c, Ring),
    ok.

test_move() ->
    Nodes = [a, c],
    Ring = create_ring(Nodes),
    Keys = lists:seq(1, 100),
    [begin
         NodeName = lookup_impl(Ring, Key),
         L = application:get_env(test_move, NodeName, []),
         application:set_env(test_move, NodeName, [Key | L])
     end || Key <- Keys],

    MoveFun = fun(NewRing, OldNode) ->
        {ok, DataList} = application:get_env(test_move, OldNode),
        [case can_move(NewRing, OldNode, Data, fun(OldNode1, NewNode1) ->  OldNode1 == NewNode1 end) of
             {true, NewNode} ->
                {ok, L} = application:get_env(test_move, OldNode),
                application:set_env(test_move, OldNode, lists:delete(Data, L)),

                NewNodeL = application:get_env(test_move, NewNode, []),
                application:set_env(test_move, NewNode, [Data | NewNodeL]);
             false -> ok
        end || Data <- DataList]
    end,

    Ring1 = create_ring([a, b, c]),
    add_node_update_impl(Ring1, [b], MoveFun),

    lists:foreach(fun(Key) ->
        Node = lookup_impl(Ring1, Key),
        {ok, L} = application:get_env(test_move, Node),
        true = lists:member(Key, L)
    end, Keys),
    ok.

test_nodes_same_config() ->
    Nodes = [{mysql1, [
        {size, 20},
        {host, "127.0.0.1"},
        {user, "root"},
        {password, ""},
        {database, "knowledge"},
        {port, 3306},
        {encoding, utf8}]},
        {mysql2, [
            {size, 10},
            {host, "192.168.70.18"},
            {user, "root"},
            {password, ""},
            {database, "knowledge"},
            {port, 3306},
            {encoding, utf8}]}
    ],
    {NodeNameList, _} = lists:unzip(Nodes),
    store_ring(test_db, NodeNameList),
    {ok, Ring} = application:get_env(?MODULE, test_db),
    Keys = lists:seq(1, 100),
    [begin
         NodeName = lookup_impl(Ring, Key),
         L = application:get_env(test_db, NodeName, []),
         application:set_env(test_db, NodeName, [Key | L])
     end || Key <- Keys],


    Nodes1 = Nodes ++ [{mysql3, [
        {size, 10},
        {host, "192.168.70.18"},
        {user, "root"},
        {password, ""},
        {database, "knowledge"},
        {port, 3306},
        {encoding, utf8}]}],
    {NodeNameList1, _} = lists:unzip(Nodes1),
    store_ring(test_db, NodeNameList1),
    MoveFun = fun(NewRing, OldNode) ->
        {ok, DataList} = application:get_env(test_db, OldNode),
        SameNodeFun = fun(OldNode1, NewNode) ->
            {_Name1, Config1} = lists:keyfind(OldNode1, 1, Nodes1),
            {_Name2, Config2} = lists:keyfind(NewNode, 1, Nodes1),
            Config1 == Config2
        end,
        [case can_move(NewRing, OldNode, Data, SameNodeFun) of
             {true, NewNode} ->
                 {ok, L} = application:get_env(test_db, OldNode),
                 application:set_env(test_db, OldNode, lists:delete(Data, L)),

                 NewNodeL = application:get_env(test_db, NewNode, []),
                 application:set_env(test_db, NewNode, [Data | NewNodeL]);
             false -> ok
         end || Data <- DataList]
    end,
    add_node_update(test_db, mysql3, MoveFun),

    {ok, Ring1} = application:get_env(?MODULE, test_db),
    lists:foreach(fun(Key) ->
        Node = lookup_impl(Ring1, Key),
        case application:get_env(test_db, Node) of
            undefined -> ok;
            {ok, L} -> true = lists:member(Key, L)
        end
    end, Keys),
    ok.

test_move2() ->
    Nodes = [{mysql1, [
        {size, 20},
        {host, "127.0.0.1"},
        {user, "root"},
        {password, ""},
        {database, "knowledge"},
        {port, 3306},
        {encoding, utf8}]},
    {mysql2, [
        {size, 10},
        {host, "192.168.70.18"},
        {user, "root"},
        {password, ""},
        {database, "knowledge"},
        {port, 3306},
        {encoding, utf8}]}
    ],
    {NodeNameList, _} = lists:unzip(Nodes),
    store_ring(test_db, NodeNameList),
    {ok, Ring} = application:get_env(?MODULE, test_db),
    Keys = lists:seq(1, 100),
    [begin
         NodeName = lookup_impl(Ring, Key),
         L = application:get_env(test_db, NodeName, []),
         application:set_env(test_db, NodeName, [Key | L])
     end || Key <- Keys],


    Nodes1 = Nodes ++ [{mysql3, [
        {size, 10},
        {host, "192.168.70.190"},
        {user, "root"},
        {password, ""},
        {database, "knowledge"},
        {port, 3306},
        {encoding, utf8}]}
        ],
    {NodeNameList1, _} = lists:unzip(Nodes1),
    store_ring(test_db, NodeNameList1),
    MoveFun = fun(NewRing, OldNode) ->
        {ok, DataList} = application:get_env(test_db, OldNode),
        SameNodeFun = fun(OldNode1, NewNode) ->
            {_Name1, Config1} = lists:keyfind(OldNode1, 1, Nodes1),
            Host1 = proplists:get_value(host, Config1),
            Database1 = proplists:get_value(database, Config1),

            {_Name2, Config2} = lists:keyfind(NewNode, 1, Nodes1),
            Host2 = proplists:get_value(host, Config2),
            Database2 = proplists:get_value(database, Config2),

            Host1 == Host2 andalso Database1 == Database2
        end,
        [case can_move(NewRing, OldNode, Data, SameNodeFun) of
             {true, NewNode} ->
                 {ok, L} = application:get_env(test_db, OldNode),
                 application:set_env(test_db, OldNode, lists:delete(Data, L)),

                 NewNodeL = application:get_env(test_db, NewNode, []),
                 application:set_env(test_db, NewNode, [Data | NewNodeL]);
             false -> ok
         end || Data <- DataList]
    end,
    add_node_update(test_db, mysql3, MoveFun),


    {ok, NewRing} = application:get_env(?MODULE, test_db),
    lists:foreach(fun(Key) ->
        Node = lookup_impl(NewRing, Key),
        case application:get_env(test_db, Node) of
            undefined -> ok;
            {ok, L} -> true = lists:member(Key, L)
        end
    end, Keys),
    ok.
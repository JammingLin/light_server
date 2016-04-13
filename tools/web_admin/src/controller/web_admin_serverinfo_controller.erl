-module(web_admin_serverinfo_controller, [Req, SessionID]).
-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

summary('GET', []) ->
    try
        ?REPLY_OK([
           {server_name, boss_session:get_session_data(SessionID, "ServerName")},
            {passport_count, get_passport_count()},
            {player_count, get_player_count()},
            {online_count, get_online_count()},
%%            {get_server_online_players, get_server_online_players()},
%%            {pvp_pool_size, get_pvp_pool_size()},
%%            {pvr_pool_size, get_pvr_pool_size()},
%%            {pvp_backup_size, get_pvp_backup_size()},
            {dgry_memory, get_dgry_memory()},
            {server_time, get_server_time()}
%%            {db_timer_status, get_db_timer_status()}
        ])
    catch
        _C:R  ->
            lager:error("summary ~p", [R]),
            ?REPLY_ERROR({R})
    end.

online_players('GET', []) ->
    Onlines = get_online_players(),
    lager:info("online players ~p", [Onlines]),
    ?REPLY_OK(Onlines).

get_server_online_players('GET', []) ->
    get_server_online_players().

pvp_players('GET', []) ->
    Players = get_pvp_players(),
    ?REPLY_OK(Players).

backup_players('GET', []) ->
    Players = get_backup_players(),
    ?REPLY_OK(Players).

add_zombie('GET', []) ->
    web_admin_base:gs_rpc(SessionID, zombie, restore_zombies, []),
    ?REPLY_JSON(ok).

clear_cache('GET', []) ->
    R = web_admin_base:gs_rpc(SessionID, cache, delete_all, []),
    lager:info("r:~p", [R]),
    ?REPLY_JSON(ok).

add_passport('POST', []) ->
    Account = case Req:post_param("account") of
                  undefined -> throw({account_undefined});
                  A -> list_to_binary(A)
              end,
    Pwd = case Req:post_param("password") of
              undefined -> <<"1">>;
              P -> list_to_binary(P)
          end,

    lager:info("add_passport: ~p, ~p", [Account, Pwd]),

    GameN = web_admin_base:game_node(SessionID),
    case web_admin_base:gs_rpc(SessionID, db_cache, get, [passport, Account]) of
        [] ->
            web_admin_base:gw_rpc(SessionID, passport, register, [GameN, Account, Pwd]);
        _ ->
            throw({account_exists})
    end,

    ?REPLY_JSON(ok).

get_passport_count() ->
    get_table_count(passport).

get_player_count() ->
    get_table_count(player).

get_table_count(TableName) ->
    Sql = lists:concat(["select count(*) as count from ", TableName]),
    [#{count := Count}] = web_admin_base:gs_rpc(SessionID, db, query, [Sql]),
    Count.

get_online_count() ->
    web_admin_base:gw_rpc(SessionID, gate_net, online_count, []).

get_pvp_pool_size() ->
    web_admin_base:gs_rpc(SessionID, mnesia, table_info, [pvp_pk_player, size]).

get_pvp_backup_size() ->
    web_admin_base:gs_rpc(SessionID, mnesia, table_info, [pvp_backup_player, size]).

get_pvr_pool_size() ->
    web_admin_base:gs_rpc(SessionID, mnesia, table_info, [pvr_pk_point, size]).

get_pvp_players() ->
    Players = web_admin_base:gs_rpc(SessionID, mnesia, dirty_select, [pvp_pk_player, [{{pvp_pk_player, '$1','$2','$3','$4','$5','$6', '$7'}, [], ['$_']}]]),
    [begin
         #{
             id => ID,
             player_id => PlayerID,
             account => get_account(PlayerID),
             pk => PK,
             create_on => CreateOn
         }
     end || {pvp_pk_player, ID, PlayerID, _Gold, _Wood, _Stone, PK, CreateOn} <- Players].

get_backup_players() ->
    Players = web_admin_base:gs_rpc(SessionID, mnesia, dirty_select, [pvp_backup_player, [{{pvp_backup_player, '$1','$2','$3'}, [], ['$_']}]]),
    [
        #{
         id => ID,
         player_id => PlayerID,
         account => get_account(PlayerID),
         create_on => CreateOn
        }
      || {pvp_backup_player, ID, PlayerID, CreateOn} <- Players].

%% 服务器内存信息
get_web_memory() ->
    Node = node(),
    get_node_memory(Node).

get_game_memory() ->
    get_node_memory(web_admin_base:game_node(SessionID)).

get_gate_memory() ->
    get_node_memory(web_admin_base:gate_node(SessionID)).

get_db_memory() ->
    get_node_memory(web_admin_base:db_node(SessionID)).

get_redis_memory() ->
    {ok, Bin} = web_admin_base:gs_rpc(SessionID, cache, q, [["info", "memory"]]),
    Text = binary_to_list(Bin),
    lager:info("text:~p", [Text]),
    V = pick_out_redis_memory(Text),
    lager:info("text:~p", [V]),
    V.

pick_out_redis_memory(Text) ->
    L = re:split(Text, "\r\n"),
    M = binary_to_list(lists:nth(2, L)),
    L2 = re:split(M, ":"),
    Bin = lists:nth(2, L2),

    round(binary_to_integer(Bin) / 1048576 * 100) / 100.

get_node_memory(Node) ->
    case rpc:call(Node, erlang, memory, []) of
        MemoryInfos when is_list(MemoryInfos) ->
            MemoryInfos1 = [{Key, bytes_to_M(Bytes)} || {Key, Bytes} <- MemoryInfos],
            #{status => ok, detail => maps:from_list(MemoryInfos1)};
        Reason ->
            lager:error("get_node_memory:~p, ~p", [Node, Reason]),
            #{status => error, detail => [], reason => tuple_to_list(Reason) }
    end.

bytes_to_M(Bytes) ->
    round(Bytes div 1048576 * 100) / 100.

get_dgry_memory() ->
    WebMemory = get_web_memory(),
%%    DbMemory = get_db_memory(),
%%    GateMemory = get_gate_memory(),
    GameMemory = get_game_memory(),
    RedisMemory = get_redis_memory(),
    #{web_memory => WebMemory,
%%        db_memory => DbMemory,
%%        gate_memory => GateMemory,
        game_memory => GameMemory,
        redis_memory => RedisMemory}.

get_online_players() ->
    Onlines = web_admin_base:gw_rpc(SessionID, gate_net, online_players, [], 30000),
    %% [[{player_id, PlayerID}, {ip, IP}],...]
    lager:info("get online players ~p ",[Onlines]),
    [begin
         Account = get_account(PlayerID),
         #{name:=Name} = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerID]),
         [{player_id, PlayerID}, {ip, IP}, {account, Account}, {name, Name}]
     end || [{player_id, PlayerID}, {ip, IP}] <- Onlines].

get_server_online_players() ->
    web_admin_base:gw_rpc(SessionID, gate_net, server_online_players, [], 30000).

get_account(PlayerID) ->
    Sql = lists:concat(["select account as account from passport where player_id=", PlayerID]),
    case web_admin_base:gs_rpc(SessionID, db, query, [Sql]) of
        [#{account := Account} | _] ->
            Account;
        _ ->
            "unknown"
    end.

get_server_time() ->
    web_admin_base:gs_rpc(SessionID, datetime, localtime, []).

get_db_timer_status() ->
    web_admin_base:db_rpc(SessionID, db_helper, get_timer_status, []).

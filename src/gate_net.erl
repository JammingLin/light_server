%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 17:02
%%%-------------------------------------------------------------------
-module(gate_net).
-author("Administrator").

-include("thrift_protocol.hrl").
-include("thrift_constants.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(online_player, {player_id, tcp_pid, proto}).

%% API
-export([init/0, bind/3, unbind/1, get_bind_info/1, is_online/1]).
-export([send/3, broadcast/2, broadcast/3, broadcast/4]).
-export([online_count/0, online_players/0]).
-export([get_ipaddress/1]).
-export([join_group/1]).


init() ->
    mnesia:start(),
    case get_gate_node() of
        undefined ->
            case mnesia:create_table(online_player, [{ram_copies, [node()]},
                {attributes, record_info(fields, online_player)}]) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, _}} -> ok
            end;
        GateNode ->
            join_group(GateNode)
    end.

join_group(GateNode) ->
    mnesia:change_config(extra_db_nodes, [GateNode]),
    Tables = [{T, mnesia:table_info(T, where_to_commit)} || T <- mnesia:system_info(tables)],
    [{Tb, mnesia:add_table_copy(Tb, node(), Type)}
        || {Tb, [{Node, Type}]} <- Tables, Node == GateNode].

get_gate_node() ->
    case init:get_argument(gate_server) of
        error -> undefined;
        {ok, [[Node]]} -> list_to_atom(Node)
    end.

%% 绑定玩家ID和socket
bind(PlayerID, TcpPid, Proto) ->
    %% TODO: 是否需要保证所有gate中的mnesia数据同步完成， 才算绑定成功
    mnesia:dirty_write(#online_player{player_id = PlayerID,
        tcp_pid = TcpPid, proto = Proto}).

%% 解除绑定
unbind(PlayerID) when is_integer(PlayerID) ->
    lager:info("unbind playerid ~p", [PlayerID]),
    mnesia:dirty_delete(online_player, PlayerID).

%% 让指定game节点的玩家都断开连接
%% disconnect(GameNode) ->
%%     Q = qlc:q([X || X <- mnesia:table(online_player), X#online_player.game_node == GameNode]),
%%     {atomic, Players} = mnesia:transaction(fun() -> qlc:e(Q) end),
%%     [begin
%%          lager:info("disconnect logout, PlayerID:~p, pid:~p, tcpid:~p", [PlayerID, TcpPid])
%% %%          gate_player:handle_gamenode_down(PlayerID, PlayerPid, TcpPid)
%%      end || #online_player{player_id = PlayerID, tcp_pid = TcpPid} <- Players].

get_bind_info(PlayerID) ->
    case mnesia:dirty_read(online_player, PlayerID) of
        [Info] ->
            {Info#online_player.player_id, Info#online_player.tcp_pid, Info#online_player.proto};
        _ -> not_online
    end.

%% 判定指定的玩家id是否在线
is_online(PlayerID) ->
    case mnesia:dirty_read(online_player, PlayerID) of
        [] -> false;
        _ -> true
    end.

%% 在线人数
online_count() ->
    mnesia:table_info(online_player, size).

online_players() ->
    {atomic, Rows} = mnesia:transaction(fun() -> mnesia:select(online_player, [{{online_player, '$1', '$2', '$3'}, [], [['$1', '$3']]}]) end),
    [begin
         {IP,_Port} = gate_net:get_ipaddress(Proto),
         [{player_id, PlayerID}, {ip, IP}]
     end || [PlayerID, Proto] <- Rows].

send(IDList, NotificationName, Data) when is_list(IDList) ->
    [send(ID, NotificationName, Data) || ID <- IDList];
send(Proto, NotificationName, Data) when is_tuple(Proto) ->
    send_reply(Proto, NotificationName, Data);
send(PlayerID, NotificationName, Data) when is_integer(PlayerID) ->
    case mnesia:dirty_read(online_player, PlayerID) of
        [#online_player{player_id = PlayerID,tcp_pid = _Pid, proto = Proto}] ->
            send_reply(Proto, NotificationName, Data);
        _ -> ok
    end.

broadcast(TargetList, Data) ->
    NotificationName = element(1, Data),
    F = fun(PlayerId) ->
        send(PlayerId, NotificationName, Data)
        end,
    lists:foreach(F, TargetList).

broadcast(SendPlayerId, NotificationName, Data) ->
    F = fun(#online_player{proto = Proto, player_id = PlayerId}, _Acc) ->
        case SendPlayerId of
            PlayerId-> ok;
            _ -> send(Proto, NotificationName, Data)
        end
        end,
    mnesia:async_dirty(fun() -> mnesia:foldl(F, [], online_player) end).

broadcast(SendPlayerId, TargetList, NotificationName, Data) ->
    F = fun(PlayerId) ->
        case SendPlayerId of
            PlayerId-> ok;
            _ -> send(PlayerId, NotificationName, Data)
        end
        end,
    lists:foreach(F, TargetList).

send_reply(Proto, NotificationName, Data) ->
    send_reply(Proto, NotificationName, Data, -1).
send_reply(Proto0, NotificationName, Data, Seqid) ->
    StructName = element(1, Data),
    ReplyMessageType = ?tMessageType_NOTIFICATION,
    ReplyType = {struct, {'game_types', StructName}},
    Reply = {{struct, [{0, ReplyType}]}, {StructName, Data}},
    try
        {Proto1, ok} = thrift_protocol:write(Proto0, #protocol_message_begin{
            name = atom_to_list(NotificationName),
            type = ReplyMessageType,
            seqid = Seqid}),
        {Proto2, ok} = thrift_protocol:write(Proto1, Reply),
        {Proto3, ok} = thrift_protocol:write(Proto2, message_end),
        {Proto4, ok} = thrift_protocol:flush_transport(Proto3),
        {Proto4, ok}
    catch
        error:{badmatch, {_, {error, _} = Error}} ->
            {Proto0, Error}
    end.

get_ipaddress(Proto) ->
    {_p, _tbp, {_bp, {_t, _tft, {_ft, {_t, _tst, {data, Socket, _i}}, _, _, _, _}}, _, _}} = Proto,
    case inet:peername(Socket) of
        {error,einval} ->
            {"0.0.0.0", 0};
        {ok, {Address, Port}} ->
            {inet:ntoa(Address), Port}
    end.

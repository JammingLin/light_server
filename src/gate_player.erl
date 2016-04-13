%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2015 上午11:36
%%%-------------------------------------------------------------------
-module(gate_player).
-author("jie").

-include("../../server_lib/include/game_types.hrl").
-include("../../server_lib/include/game_constants.hrl").

%% API
-export([init/1]).
-export([handle_function/2, handle_error/2, handle_info/3, terminate/1]).
-export([handle_heartbeat_timeout/0, handle_reduse_connect/1]).

init(Proto) ->
    put(thrift_proto, Proto),
    ok.

%% 获取协议版本号
%% 获取协议版本号
handle_function(get_protocol_version, _Parms) ->
    lager:info("*******get_protocol_version"),
    {reply, ?game_Protocol_version};

handle_function(get_message, _Parms) ->
    io:format("handle function get_message, return pong~n"),
    {reply, <<"pong">>};

handle_function(login, {ProtocalVer, Account, Password}) ->
    lager:info("*******player login ~p ~p pid:~p ", [Account, Password, self()]),
    Proto = get(thrift_proto),
    check_protocal_version(ProtocalVer),
    PlayerID = player_session:account_login(Proto, Account, Password),
    {reply, PlayerID};

handle_function(login_by_device, {ProtocalVer, DeviceToken, Platform, Area}) ->
    Proto = get(thrift_proto),
    check_protocal_version(ProtocalVer),
    PlayerID = player_session:device_login(Proto, DeviceToken, Platform, Area),
    {reply, PlayerID};

handle_function(login_by_gamecenter, {ProtocalVer, UserID, Name, Password, AccountPlatform, Area}) ->
    Proto = get(thrift_proto),
    check_protocal_version(ProtocalVer),
    PlayerID = player_session:account_login_by_game_center(Proto, UserID, Name, Password, AccountPlatform, Area),
    {reply, PlayerID};

%% 发起重连
handle_function(reconnect, {Account, Password}) ->
    Proto = get(thrift_proto),
    PlayerID = player_session:account_login(Proto, Account, Password),
    {reply, PlayerID};

handle_function(reconnect_by_device, {DeviceToken, Platform, Area}) ->
    Proto = get(thrift_proto),
    PlayerID = player_session:device_login(Proto, DeviceToken, Platform, Area),
    {reply, PlayerID};

handle_function(logout, _Params) ->
    player_session:logout();

handle_function(heartbeat, _Params) ->
    lager:debug("handle_function heartbeat current onlines:~p", [gate_net:online_count()]),
    player_session:cancel_heartbeat_timer(),
    player_session:start_heartbeat_timer(),
    ok;

handle_function(Function, Params) ->
    game_player:Function(Params).

handle_error(_Function, timeout) ->
%%     lager:debug("handle error reason:timeout"),
    ok;
handle_error(_Function, closed) ->
    lager:debug("handle error reason:closed"),
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, closed);

%% 处理玩家进程非异常退出或者被顶号踢出后，gate的退出。
handle_error(exit, {FromPid, Reason}) ->
    lager:debug("handle error exit from:~p, reason:~p", [FromPid, Reason]),
    PlayerID = get(player_id),
    %% 处理游戏中进程切换

    player_session:clean_session(PlayerID, Reason);

%% 处理玩家被踢出。
handle_error(kickout, {FromPid, Reason}) ->
    lager:debug("handle error kickout from:~p, reason:~p", [FromPid, Reason]),
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, Reason);

%% 处理玩家进程异常、玩家进程心跳超时、gate异常，gate的退出。
handle_error(_Function, Reason) ->
    lager:debug("handle error function(~p), reason:~p", [_Function, Reason]),
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, Reason).

handle_info({kickout, another_device_logined, FromPid}, Req, State) ->
    handle_error(exit, {FromPid, another_device_logined}),
    {stop, Req, State};
handle_info({'EXIT', _Pid, noconnection}, Req, State) ->
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, noconnection),
    {stop, Req, State}.

handle_heartbeat_timeout() ->
    lager:debug("handle_heartbeat_timeout."),
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, heartbeat_timeout).

%% 拒绝玩家socket连接
handle_reduse_connect(Reason) ->
    exit(self(), Reason).

terminate({crash,exit,another_device_logined})->
    ok;
terminate(Reason)->
    lager:debug("terminate, reason:~p", [Reason]),
    PlayerID = get(player_id),
    player_session:clean_session(PlayerID, Reason).

check_protocal_version(ClientVer) ->
    case ClientVer == ?game_Protocol_version of
        true -> ok;
        false ->
            throw(#login_exception{ error_code = ?game_login_exception_errcode_WRONG_PROTOCAL,
                msg = integer_to_list(?game_Protocol_version)})
    end.
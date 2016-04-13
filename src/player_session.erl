%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2015 下午2:11
%%%-------------------------------------------------------------------
-module(player_session).
-author("chendh").
-include("game_types.hrl").

-define(MAX_CONNS, 100000). %% 最大连接数
-define(HEARTBEAT_TIMEOUT, 3 * 60 * 1000).
-define(DEFAULT_AREA, <<"CN">>).
%% API
-compile(export_all).

%% 返回PlayerID
account_login_by_game_center(Proto, UserID, Name, _Password, AccountPlatform, Area)->
    lager:info("gamecenter ~p ~p ~p", [UserID, Name, AccountPlatform]),
    check_game_server_is_full(),

    PlayerID = passport:get_device_gamecenter_player_id(UserID, AccountPlatform),

    kickout_online_session(PlayerID),
    login_success(PlayerID, Name, Area, Proto),
    PlayerID.

account_login(Proto, Account, Password) ->
    check_game_server_is_full(),
    PlayerID = passport:get_player_id(Account, Password),
    kickout_online_session(PlayerID),
    login_success(PlayerID, Account, ?DEFAULT_AREA, Proto),
    PlayerID.

%% 返回PlayerID(为新设备创建账号)
device_login(Proto, DeviceToken, Platform, Area) ->
    Account = passport:make_device_account(DeviceToken, Platform),

    check_game_server_is_full(),
    PlayerID = passport:get_device_player_id(DeviceToken, Platform),
    kickout_online_session(PlayerID),
    login_success(PlayerID, Account, Area, Proto),
    PlayerID.

%% 尝试踢掉已在线会话
kickout_online_session(PlayerID) ->
    case gate_net:is_online(PlayerID) of
        true ->
            lager:info("new player:~p,pid:~p",[PlayerID, self()]),
            notify_and_kickout(PlayerID);
        false ->
            ok
    end.

%% 通知上一个玩家被踢出
notify_and_kickout(PlayerID) ->
    {PlayerID, OldPid, OldProto} = gate_net:get_bind_info(PlayerID),
    EmptyMsg = #another_device_logined{ip_address = "", port = 0},
    lager:info("kickout player:~p,pid:~p",[PlayerID, OldPid]),
    gate_net:send(OldProto, another_device_logined, EmptyMsg),
    OldPid!{kickout, another_device_logined, self()}.

login_success(PlayerID, Account, Area, Proto) ->
    start_player_and_bind_with_area(PlayerID, Account, Area, Proto),
    start_heartbeat_timer().

%% add by linzhiwei 20150326
%% 获取玩家最大进程数
get_game_max() ->
    case init:get_argument(game_max_num) of
        error -> ?MAX_CONNS;
        {badrpc, _} -> ?MAX_CONNS;
        {ok, [[MaxNum]]} -> list_to_integer(MaxNum)
    end.

%% 检查当前玩家连接数是否已满
check_game_server_is_full() ->
    CurOnlines = gate_net:online_count(),
    Max = get_game_max(),
    lager:debug("ensure game server not full max : ~p, CurOnlines:~p", [Max, CurOnlines]),
    if CurOnlines >= Max ->
        throw(#login_exception{
            error_code = ?game_login_exception_errcode_GAME_SERVER_FULL,
            msg = ""});
        true ->
            ok
    end.

start_heartbeat_timer() ->
    PlayerID = get(player_id),
    GamePlayerPid = get(player_pid),
    lager:debug("start_heartbeat_timer() ~p ~p ~p", [self(), PlayerID, GamePlayerPid]),
    TRef1 = erlang:send_after(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
    put(heartbeat_tref, TRef1).

%%
cancel_heartbeat_timer() ->
    case get(heartbeat_tref) of
        undefined ->
            lager:info("get heartbeat_tref undefined.");
        TRef ->
            erlang:cancel_timer(TRef)
    end.

%% TCP 自己退出
exit_self_tcp(Reason) ->
    PlayerID = get(player_id),
    case Reason of
        another_device_logined -> ok;
        _ ->
        clean_session(PlayerID, Reason)
    end,
    exit(Reason).

%% 清除会话
%% 1.停止心跳定时;  2.解绑在线信息;
clean_session(PlayerID, _Reason) ->
    lager:info("self pid ~p reason ~p", [self(), _Reason]),
    cancel_heartbeat_timer(),
    case gate_net:get_bind_info(PlayerID) of
        not_online ->
            ok;
        {PlayerID, CurSelf, _Proto} ->
            if
                CurSelf =:= self() ->
                    update_last_leave_time(PlayerID),
                    lager:info("clean session ~p ~p", [PlayerID, _Reason]),
                    gate_net:unbind(PlayerID);
                true ->
                    ok
            end
    end.

start_player_and_bind_with_userinfo(PlayerID, UserInfo, Proto) ->
    start_player_with_userinfo(PlayerID, UserInfo),
    player:init(PlayerID, UserInfo),
    gate_net:bind(PlayerID, self(), Proto).

start_player_and_bind_with_area(PlayerID, Account, Area, Proto) ->
    start_player_with_area(PlayerID, Account, Area),
    player:init(PlayerID, Account, Area, Account),
    gate_net:bind(PlayerID, self(), Proto).

start_player(PlayerID, Account) when PlayerID > 0 ->
    start_player_with_area(PlayerID, Account, ?DEFAULT_AREA).

start_player_with_userinfo(PlayerId, UserInfo)->
    start_player_with_area(
        PlayerId,
        maps:get(user_name, UserInfo),
        <<"CN">>
    ).

start_player_with_area(PlayerID, Account, Area) when PlayerID > 0 ->
    put(player_id, PlayerID),
    put(player_account, Account),
    put(player_area, Area),
    ok.

%% 玩家主动退出
logout() ->
    PlayerID = get(player_id),
    lager:debug("logout:~p", [PlayerID]),
    case PlayerID of
        undefined -> ok;
        _ ->
            case gate_net:is_online(PlayerID) of
                true ->
                    exit_self_tcp(player_logout);
                false -> ok
            end
    end.

%% 更新玩家最后离开时间
update_last_leave_time(PlayerID) ->
    Ret = player_status:update_last_leave_time(PlayerID, datetime:localtime()),
    lager:debug("update_last_leave_time:~p", [Ret]).
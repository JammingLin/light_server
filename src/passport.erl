%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2014 16:35
%%%-------------------------------------------------------------------
-module(passport).
-author("long").
-define(MAX_CONNS, 100000). %% 最大连接数
-define(RPC_TIMEOUT, 5 * 1000).
-include("game_types.hrl").

%%% 通行证模块, 这个在正式环境中一般排不上用场

%% API
-export([register/2,
    get_player_id/2,
    get_device_player_id/2,
    get_device_gamecenter_player_id/2,
    make_device_account/2,
    make_gamecenter_account/2]).

%% 注册一个玩家
register(Account, Password) ->
    ID = id_generator:get_id(passport),
    true = is_integer(ID),
    Passport = #{account=>Account, password=>Password, player_id=>ID},
    true = db_cache:save(passport, Passport),
    ID.

%% 获取玩家ID;  账号不存在或密码错误时抛异常
get_player_id(Account, Password) ->
    Passport = get_passport(Account),
    check_passport(Passport),
    check_password(Passport, Password),
    maps:get(player_id, Passport).

%% 根据设备标识获取玩家id，不存在的设备则添加
get_device_player_id(DeviceToken, Platform) ->
    Passport = get_device_passport(DeviceToken, Platform),
    maps:get(player_id, Passport).

get_device_gamecenter_player_id(Account, Platform) ->
    Passport = get_device_gamecenter_passport(Account, Platform),
    maps:get(player_id, Passport).

%% 获取账号
get_passport(Account) ->
    db_cache:get(passport, Account).

get_device_gamecenter_passport(Account, Platform) ->
    Passport = device_token:ensure_device_passport(Account, Platform, fun make_gamecenter_account/2),
    Passport.

%% 获取设备账号
get_device_passport(DeviceToken, Platform) ->
    device_token:ensure_device_passport(DeviceToken, Platform, fun make_device_account/2).

make_gamecenter_account(Account, Platform) ->
    lists:concat([binary_to_list(Account), "@@", binary_to_list(Platform)]).

make_device_account(DeviceToken, Platform) ->
    lists:concat([binary_to_list(DeviceToken), "@@", Platform]).

%% 检验账号
check_passport([]) ->
    throw(#login_exception{
        error_code = ?game_login_exception_errcode_UNKNOW_ACCOUNT,
        msg = ""});
check_passport({badrpc,nodedown}) ->
    throw(#login_exception{
        error_code = ?game_login_exception_errcode_GAME_SERVER_OFFLINE,
        msg = ""});
check_passport(P) when is_map(P) ->
    ok.

%% 检验密码
check_password(#{password := Pwd} = _Passport, Password) when Pwd =/= Password ->
    throw(#login_exception{
        error_code = ?game_login_exception_errcode_WRONG_PASSWORD,
        msg = ""});
check_password(#{password := Password}, Password) ->
    ok.


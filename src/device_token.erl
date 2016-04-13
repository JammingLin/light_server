%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 一月 2015 11:38
%%%-------------------------------------------------------------------
-module(device_token).
-author("Administrator").
-define(DEVICE_PASSPORT_PWD, "knowledge").

%% API
-export([ensure_device_passport/3, save_device_token/3]).

%% 根据设备标识获取玩家账号，不存在的设备则添加
ensure_device_passport(DeviceToken, Platform, FunMakeAccount) ->
    lager:info("passport ~p ~p ~p", [DeviceToken, Platform, FunMakeAccount]),
    Account = FunMakeAccount(DeviceToken, Platform),
    case db_cache:get(passport, Account) of
        #{player_id := _PlayerID} = Passport ->
            Passport;
        [] ->
            PlayerID = id_generator:get_id(passport),
            NewPassport = #{account => Account, password => ?DEVICE_PASSPORT_PWD, player_id => PlayerID,  create_on => datetime:localtime()},
            db_cache:save(passport, NewPassport),
            %save_device_token(DeviceToken, Platform, PlayerID),
            NewPassport
    end.

save_device_token(DeviceToken, Platform, PlayerID) ->
    Data = #{token => DeviceToken, platform => Platform, player_id => PlayerID},
    db_cache:save(device_token, Data).
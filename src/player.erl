%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2015 上午11:04
%%%-------------------------------------------------------------------
-module(player).
-author("linzhiwei").
-include("game_types.hrl").
-include("db_ets.hrl").
-define(OPEN_LOGIN_AWARD, 1).
-define(TOPIC_TOP, 3).
%% API
-export([init/4, create_player/3, get_player/1, set_player_name/2]).
-compile(export_all).

init(PlayerID, Account, Area, NickName) ->
    put(player_id, PlayerID),
    put(player_account, Account),
    trans:nolock_execute(fun() ->
        case load_player_data(PlayerID, NickName, Area) of
            {first_login, _PlayerData} ->
                ok;
            _ -> ok
        end
    end),
    proc_login_success(PlayerID).

init(PlayerID, UserInfo) ->
    trans:nolock_execute(fun() ->
        case load_player_data(PlayerID, UserInfo) of
            {first_login, _PlayerData} ->
                ok;
            _ -> ok
        end
                         end),
    proc_login_success(PlayerID).

proc_login_success(PlayerID) ->
    lager:debug("proc_login_success start"),
    Now = datetime:localtime(),
    player_status:update_last_logon_time(PlayerID, Now),
    event:send(login_success, [PlayerID]),
    lager:debug("proc_login_success end").

fix_player_name(Name) ->
    case utils:utf8_string_length(Name) =< 15 of
        true ->
            Name;
        false ->
            "newbie"
    end.

get_init_player_data() ->
    tplt_data:get(init_player_data).

%% 创建玩家角色
create_player(PlayerID, NickName, Area) when is_integer(PlayerID), PlayerID > 0 ->
    InitPlayerData = get_init_player_data(),
    PlayerData = InitPlayerData#{
        player_id => PlayerID,
        name => NickName,
        area => Area
        },
    lager:info("create player ~p", [PlayerData]),
    db_cache:save(player, PlayerData),
    PlayerData.

create_player(PlayerID, UserInfo) when is_integer(PlayerID), PlayerID > 0 ->
    InitPlayerData = get_init_player_data(),
    PlayerData = InitPlayerData#{
        player_id => PlayerID,
        name => maps:get(name, UserInfo),
        area => <<"CN">>
        },
    db_cache:save(player, PlayerData),
    PlayerData.

load_player_data(PlayerID, NickName, Area) when is_integer(PlayerID), PlayerID > 0 ->
    case db_cache:get(player, PlayerID) of
        [] -> {first_login, create_player(PlayerID, NickName, Area)};
        Record -> Record
    end.

load_player_data(PlayerID, UserInfo) when is_integer(PlayerID), PlayerID > 0 ->
    case db_cache:get(player, PlayerID) of
        [] -> {first_login, create_player(PlayerID, UserInfo)};
        Record -> Record
    end.

%% 获取玩家数据
get_player(PlayerID) when is_integer(PlayerID), PlayerID > 0 ->
    case db_cache:get(player, PlayerID) of
        [] -> [];
        Data ->
            translate:to_record(player, Data)
    end.

%% 设置玩家名称
set_player_name(PlayerID, NewPlayerName) when is_integer(PlayerID) ->
    lager:info("~p", [NewPlayerName]),
    LetterCount = utils:utf8_string_length(NewPlayerName),
    case LetterCount >= 2 andalso LetterCount =< 15 of %% 限制最小长度，2~15个字，字母
        true ->
            ok;
        false ->
            throw(#set_player_name_exception{
                error_code = ?game_set_player_name_errcode_ILLIGAL_NAME_LENGTH,
                msg = integer_to_list(LetterCount)})
    end,

    case db_cache:get(player, PlayerID) of
        [] ->
            throw(#set_player_name_exception{
                error_code = ?game_set_player_name_errcode_PLAYER_NOT_FOUND,
                msg = integer_to_list(PlayerID)
            });
        PlayerData ->
            PlayerData1 = PlayerData#{name := NewPlayerName},
            db_cache:save(player, PlayerData1) > 0
    end.

get_area_list()->
    Result = tplt_data:get_all(area),
    [begin
         binary_to_list(maps:get(ab, R))
     end || R <- Result, binary_to_list(maps:get(ab, R)) =/= []].
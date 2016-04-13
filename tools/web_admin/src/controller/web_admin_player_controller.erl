%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2015 下午5:01
%%%-------------------------------------------------------------------
-module(web_admin_player_controller, [Req, SessionID]).
-author("chendh").

%% API
-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    ?REPLY_OK([]);
index('GET', [PlayerID]) ->
    case re:run(PlayerID, "^[\\d]+$") of
        {match, _} ->
            ?REPLY_OK(#{player_id => PlayerID});
        _ ->
            ?REPLY_OK([])
    end.


get_player('POST', []) ->
    SearchMode = Req:post_param("mode"),
    SearchKey = Req:post_param("key"),
    Data = handle_get_player(SearchMode, utils:safe_sql(SearchKey)),
    ?REPLY_JSON(Data).

save_player('POST', []) ->
    try
        PlayerID = list_to_integer(Req:post_param("player_id")),
        handle_save_player(PlayerID,
            list_to_integer(Req:post_param("level")),
            list_to_integer(Req:post_param("bean"))),
        {json, maps:to_list(#{code => 1, msg => "", data => null})}
    catch
        _Class:Reason  ->
            ?REPLY_JSON({error, 500, Reason})
    end.

clear_passport('POST', []) ->
    PlayerIDValue = Req:post_param("player_id"),
    PlayerID = list_to_integer(PlayerIDValue),
    true = web_admin_base:gs_rpc(SessionID, channel_passport, handle_clear_passport, [PlayerID]),
    ?REPLY_JSON([]).

replace_player('POST', []) ->
    OldPlayerID = list_to_integer(Req:post_param("old_player_id")),
    NewPlayerID = list_to_integer(Req:post_param("new_player_id")),
    ok = web_admin_base:gs_rpc(SessionID, channel_passport, replace_player, [OldPlayerID, NewPlayerID]),
    ?REPLY_JSON([]).

make_zombie('POST', []) ->
    PlayerIDValue = Req:post_param("player_id"),
    PlayerID = list_to_integer(PlayerIDValue),
    [ok] = web_admin_base:gs_rpc(SessionID, zombie, make_zombie_tplts, [[PlayerID]]),
    ?REPLY_JSON([]).

get_city_data('GET', [PlayerID]) ->
    Data = handle_get_city_data(PlayerID),
    Data1 = [begin
                 D1 = maps:update(settle_time, maps:to_list(ST), D),
                 maps:update(finish_time, maps:to_list(FT), D1)
             end || #{settle_time:=ST, finish_time:=FT}=D <- Data],
    ?REPLY_JSON(Data1).

get_world_data('GET', [PlayerID]) ->
    Data = handle_get_world_data(PlayerID),
    ?REPLY_JSON(Data).

get_status_data('GET', [PlayerID]) ->
    Data = handle_get_status_data(PlayerID),
    ?REPLY_JSON(Data).

get_camp_data('GET', [PlayerID]) ->
    Data = handle_get_camp_data(PlayerID),
    ?REPLY_JSON(Data).

%% Handlers

get_passport(PlayerIDText) when is_list(PlayerIDText) ->
    get_passport(list_to_integer(PlayerIDText));
get_passport(PlayerID) ->
    Sql = lists:concat(["select account,create_on from passport where player_id=", PlayerID]),
    case web_admin_base:gs_rpc(SessionID, db, query, [Sql]) of
        [] ->
            [];
        [Passport | _] ->
            Passport
    end.

handle_get_player("player_id", PlayerIDText) ->
    Rtn = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerIDText]),
    Company = web_admin_base:gs_rpc(SessionID, db_cache, get, [player_company, PlayerIDText]),
    case Rtn of
        [] ->
            {error, 400, "player not exist"};
        {error, Reason} ->
            {code, 500, Reason};
        _ ->
            Rtn#{account => case get_passport(PlayerIDText) of
                                [] -> "";
                                #{account:=A} ->
                                    A
                            end
                }
    end;
handle_get_player("player_account", Account) ->
    PlayerID = handle_get_player_id(Account),
    Rtn = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerID]),
    Company = web_admin_base:gs_rpc(SessionID, db_cache, get, [player_company, PlayerID]),
    case Rtn of
        [] ->
            {error, 400, "player not exist"};
        {error, Reason} ->
            {code, 500, Reason};
        _ ->
            Rtn#{account => Account}
    end;

handle_get_player("player_name", Name) ->
    PageSize = list_to_integer(Req:post_param("page_size")),
    PageNo = list_to_integer(Req:post_param("page_no")),
    Skip = (PageNo - 1) * PageSize,

    Sql = lists:concat(["Select player_id from player where name like '%", Name, "%' order by level desc limit ", Skip, ",", PageSize]),
    SqlTotal = lists:concat(["Select count(*) as total from player where name like '%", Name, "%' "]),
    Rtn = web_admin_base:gs_rpc(SessionID, db, query, [Sql]),
    [#{total := Total}] = web_admin_base:gs_rpc(SessionID, db, query, [SqlTotal]),
    case Rtn of
        [] ->
            {error, 400, "player not exist"};
        {error, Reason} ->
            {code, 500, Reason};
        _ ->
            Rows = [begin
                 P = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerID]),
                 P#{
                     account => case get_passport(PlayerID) of
                                   [] -> "";
                                   #{account:=A} ->
                                       A
                               end
                 }
            end || #{player_id:=PlayerID} <- Rtn],
            #{total => Total, rows => Rows}
    end;
handle_get_player(_UnknownSearchMode, _SearchKey) ->
    lager:warning("_UnknownSearchMode:~p", [_UnknownSearchMode]),
    ok.

handle_get_player_id(Account) ->
    case web_admin_base:gs_rpc(SessionID, db_cache, get, [passport, Account]) of
        #{player_id:=PlayerID} ->
            PlayerID;
        _ ->
             -1
    end.
%%
handle_save_player(PlayerID, Level, Pk) ->
     Player = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerID]),
     OldLevel = maps:get(level, Player),
     OldExp = maps:get(experience, Player, 0),
     Player1 = case OldLevel == Level of
         true ->
             Player;
         false ->
             #{exp:=NewExp} = web_admin_base:gs_rpc(SessionID, tplt_data, get, [level_exp, Level]),
             web_admin_base:gs_rpc(SessionID, player_resource, add_exp, [Player, NewExp - OldExp])
     end,
    Player2 = #{player_id => maps:get(player_id, Player1),
        name => maps:get(name, Player1),
        level => maps:get(level, Player1),
        experience => maps:get(experience, Player1),
        area => maps:get(area, Player1)},
        %% chicago 编译器 还不支持 map 更新语法
        %% Player2 = #{diamond := Diamond, gold := Gold, wood := Wood, stone := Stone, iron := Iron},
     Rtn = web_admin_base:gs_rpc(SessionID, db_cache, save, [player, Player2]),
     lager:debug("save player rtn:~p", [Rtn]).

handle_get_city_data(PlayerID) ->
    Data = web_admin_base:gs_rpc(SessionID, player_city, get_city_data, [PlayerID]),
    Data.

handle_get_world_data(PlayerID) ->
    Data = web_admin_base:gs_rpc(SessionID, player_world, get_islands, [PlayerID]),
    Data.

handle_get_status_data(PlayerID) ->
    #{create_on:=CreateOn} = get_passport(PlayerID),
    Data = web_admin_base:gs_rpc(SessionID, player_status, get_player_status, [PlayerID]),
    Data#{create_on => CreateOn}.

handle_get_camp_data(PlayerID) ->
    Data = web_admin_base:gs_rpc(SessionID, db_cache, get, [attack_camp, PlayerID]),
    Data.

handle_modify_newbie(#{player_account := Account, main_step := MainStep, sub_step := SubStep}) ->
    PlayerID = handle_get_player_id(Account),
    Rtn = web_admin_base:gs_rpc(SessionID, db_cache, get, [player, PlayerID]),
    case Rtn of
        [] ->
            {error, 400, "player not exist"};
        {error, Reason} ->
            {code, 500, Reason};
        _ ->
            #{player_id:=PlayerID} = Rtn,
            true = web_admin_base:gs_rpc(SessionID, newbie_guide, handle_update_newbie_force_task, [PlayerID, {force_task, MainStep, SubStep}])
    end.
%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2015 下午4:16
%%%-------------------------------------------------------------------
-module(web_admin_data_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

newbie('GET', []) ->
    {ok, []}.

modify_newbie_force('POST', []) ->
    try
        PlayerID = list_to_integer(Req:post_param("player_id")),
        MainStep = list_to_integer(Req:post_param("main_step")),
        SubStep = list_to_integer(Req:post_param("sub_step")),
        handle_modify_newbie_force_task(PlayerID, MainStep, SubStep),
        ?REPLY_JSON(ok)
    catch
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

modify_newbie_trigger('POST', []) ->
    try
        PlayerID = list_to_integer(Req:post_param("player_id")),
        TriggerTaskID = list_to_integer(Req:post_param("trigger_task_id")),
        Status = list_to_integer(Req:post_param("status")),
        handle_update_newbie_trigger_task(PlayerID, TriggerTaskID, Status),
        ?REPLY_JSON(ok)
    catch
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

handle_get_player_id(Account) ->
    case rpc:call(web_admin_base:game_node(SessionID), db_cache, get, [passport, Account], 1000) of
        #{player_id:=PlayerID} ->
            PlayerID;
        _ ->
            throw({error, 400, "passport not exist"})
    end.

handle_get_player(Account) ->
    PlayerID = handle_get_player_id(Account),
    Rtn = rpc:call(web_admin_base:game_node(SessionID), db_cache, get, [player, PlayerID]),
    case Rtn of
        [] ->
            throw({error, 400, "player not exist"});
        {error, Reason} ->
            throw({code, 500, Reason});
        _ ->
            Rtn
    end.

handle_modify_newbie(#{player_account := Account, main_step := MainStep, sub_step := SubStep}) ->
    #{player_id:=PlayerID} = handle_get_player(Account),
    true = rpc:call(web_admin_base:game_node(SessionID), newbie_guide, handle_update_newbie_force_task, [PlayerID, {force_task, MainStep, SubStep}]).

handle_modify_newbie_force_task(PlayerID, MainStep, SubStep) ->
    true = rpc:call(web_admin_base:game_node(SessionID), newbie_guide, handle_update_newbie_force_task, [PlayerID, {force_task, MainStep, SubStep}]).

handle_update_newbie_trigger_task(PlayerID, TriggerTaskID, Status) ->
    true = rpc:call(web_admin_base:game_node(SessionID), newbie_guide, handle_update_newbie_trigger_task, [PlayerID, {trigger_task, TriggerTaskID, Status}]).

pvp_pvr('GET', []) ->
    lager:error("pvp_pvr load."),
    ?REPLY_OK("");
pvp_pvr('POST', []) ->
    Action = Req:query_param("action"),
    PlayerAccount = Req:post_param("player_account"),
    try
        case Action of
            "pvp" ->
                handle_add_to_pvp(PlayerAccount);
            "pvr" ->
                handle_add_to_pvr(PlayerAccount);
            _ ->
                ?REPLY_ERROR("unknow action")
        end
    catch
        throw:Error ->
            ?REPLY_OK(Error);
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

rand_pvp('GET', []) ->
    lager:error("rand_pvp load."),
    ?REPLY_OK("");
rand_pvp('POST', []) ->
    AttackersValue = Req:post_param("attackers"),
    DefendersValue = Req:post_param("defenders"),
    Attackers = utils:split(AttackersValue, $,),
    Defenders = utils:split(DefendersValue, $,),
    lager:info("Attackers:~p", [Attackers]),
    lager:info("Defenders:~p", [Defenders]),
    try
        Ret = handle_rand_pvp(Attackers, Defenders),
        lager:info("Ret:~p", [Ret]),
        ?REPLY_OK(ok)
    catch
        throw:Error ->
            ?REPLY_OK(Error);
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

rand_pvp_2('POST', []) ->
    AStart = list_to_integer(Req:post_param("astart")),
    AEnd = list_to_integer(Req:post_param("aend")),
    DStart = list_to_integer(Req:post_param("dstart")),
    DEnd = list_to_integer(Req:post_param("dend")),
    try
        handle_rand_pvp(AStart, AEnd, DStart, DEnd),
        ?REPLY_OK(ok)
    catch
        throw:Error ->
            ?REPLY_OK(Error);
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

rand_pve('GET', []) ->
    lager:error("rand_pve load."),
    ?REPLY_OK("");
rand_pve('POST', []) ->
    AttackersValue = Req:post_param("attackers"),
    LevelsValue = Req:post_param("levels"),
    Attackers = utils:split(AttackersValue, $,),
    Levels = [list_to_integer(L) || L <- utils:split(LevelsValue, $,)],
    OnlyEmpty = Req:post_param("chk_only_empty"),
    lager:info("Attackers:~p", [Attackers]),
    lager:info("Levels:~p", [Levels]),
    lager:info("Levels:~p", [OnlyEmpty]),
    try
        Ret = handle_rand_pve(Attackers, Levels, OnlyEmpty == "1"),
        lager:info("Ret:~p", [Ret]),
        ?REPLY_OK(ok)
    catch
        throw:Error ->
            ?REPLY_OK(Error);
        _Class:Reason  ->
            ?REPLY_OK({error, 500, Reason, erlang:get_stacktrace()})
    end.

handle_add_to_pvp(Account) ->
    PlayerID = handle_get_player_id(Account),
    Rtn = rpc:call(web_admin_base:game_node(SessionID), db_cache, get, [player, PlayerID]),
    case Rtn of
        [] ->
            {error, 400, "player not exist"};
        {error, Reason} ->
            {code, 500, Reason};
        _ ->
            #{player_id:=PlayerID} = Rtn,
            ok = rpc:call(web_admin_base:game_node(SessionID), pvp, add_to_pool, [Rtn, init])
    end.

handle_add_to_pvr(Account) ->
    PlayerID = handle_get_player_id(Account),
    PlayerWorld = rpc:call(web_admin_base:game_node(SessionID), db_cache, get, [player_world, PlayerID]),
    [begin
         Point = rpc:call(web_admin_base:game_node(SessionID), resource_point, get_point, [PointID]),
         Ret = rpc:call(web_admin_base:game_node(SessionID), pvr, add_to_pool, [Point]),
         lager:error("add point:~p, ~p", [Point, Ret])
     end || #{enemy_type:=7, level_player_id:=PointID} <- PlayerWorld],
    ok.

handle_rand_pvp(Attackers, Defenders) ->
    rpc:call(web_admin_base:gate_node(SessionID), gm_command, rand_pvp, [web_admin_base:game_node(SessionID), Attackers, Defenders]).

handle_rand_pvp(AStart, AEnd, DStart, DEnd) ->
    rpc:call(web_admin_base:gate_node(SessionID), gm_command, rand_pvp, [web_admin_base:game_node(SessionID), AStart, AEnd, DStart, DEnd]).

handle_rand_pve(Attackers, Levels, OnlyEmpty) ->
    rpc:call(web_admin_base:gate_node(SessionID), gm_command, rand_pve, [web_admin_base:game_node(SessionID), Attackers, Levels, OnlyEmpty]).

switch_players('GET', []) ->
    ?REPLY_OK([]);
switch_players('POST', []) ->
    Player1ID = list_to_integer(Req:post_param("player1_id")),
    Player1Channel = list_to_integer(Req:post_param("player1_channel")),
    Player2ID = list_to_integer(Req:post_param("player2_id")),
    Player2Channel = list_to_integer(Req:post_param("player2_channel")),
    ok = web_admin_base:gs_rpc(SessionID, channel_passport, switch_players,
        [Player1ID, Player1Channel, Player2ID, Player2Channel]),
    ?REPLY_OK([]).

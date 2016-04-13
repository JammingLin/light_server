%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2015 下午3:10
%%%-------------------------------------------------------------------
-module(gm_command).
-author("jie").
-define(SECONDS_PER_DAY, 86400).        %% 每天的秒数
-define(EMPTY_TIME, datetime:make_datetime(1900, 1, 1, 0, 0, 0)).

%% API
-export([start/1, modify_resource/8, modify_research/4, modify_building/6, modify_army/4]).
-export([modify_pve/4, modify_pvp/4, modify_newbie_maintask/4, modify_newbie_triggertask/4]).
-export([modify_world_fog/3]).
-export([rand_pvp/5, rand_pvp/3]).

start(GameNode) ->
    pong = net_adm:ping(GameNode).

%% 修改资源数据
modify_resource(GameNode, Account, Diamond, Gold, Wood, Stone, Exp, PK) ->
    PlayerData = get_player(GameNode, Account),
    NewGold = case Gold of
                  0 -> maps:get(gold, PlayerData);
                  _ -> Gold
              end,
    NewDiamond = case Diamond of
                     0 -> maps:get(diamond, PlayerData);
                     _ -> Diamond
                 end,
    NewWood    = case Wood of
                     0 -> maps:get(wood, PlayerData);
                     _ -> Wood
                 end,
    NewStone   = case Stone of
                     0 -> maps:get(stone, PlayerData);
                     _ -> Stone
                 end,
    NewPK      = case PK of
                     0 -> maps:get(pk, PlayerData);
                     _ -> PK
                 end,
    NewExp     = case Exp of
                     0 -> maps:get(experience, PlayerData);
                     _ -> Exp
                 end,
    PlayerData1 = PlayerData#{gold:=NewGold, diamond:=NewDiamond, wood:=NewWood, stone:=NewStone, experience:=NewExp, pk:= NewPK},
    PlayerData2 = rpc:call(GameNode, player_resource, add_exp, [PlayerData1, Exp]),
    rpc:call(GameNode, db_cache, save, [player, PlayerData2]).

%% 修改科研数据
modify_research(GameNode, Account, ResearchType, Level) ->
    PlayerData = get_player(GameNode, Account),
    PlayerID = maps:get(player_id, PlayerData),
    #{init_level := InitLevel} = rpc:call(GameNode, tplt_data, get, [research, ResearchType]),
    [rpc:call(GameNode, player_research, handle_immediately_finish_research, [PlayerID, ResearchType])
        || _ <- lists:seq(InitLevel, Level)].

%% 修改建筑数据
modify_building(GameNode, Account, HoleID, BuildingID, X, Y) ->
    PlayerData = get_player(GameNode, Account),
    PlayerID = maps:get(player_id, PlayerData),
    PlayerBuilding = #{player_id=>PlayerID, hole_id=>HoleID, building_id=>BuildingID, time_type=>0,
        finish_time=>datetime:make_datetime(1900, 1, 1, 0, 0, 0), settle_time=>datetime:make_datetime(1900, 1, 1, 0, 0, 0),
        temp_depot=>0, award=>0},
    rpc:call(GameNode, db_cache, save, [player_city, PlayerBuilding]),
    rpc:call(GameNode, force_recruit, handle_construct_finish, [PlayerID, BuildingID div 100, HoleID]),
    rpc:call(GameNode, defence_system, handle_construct_start, [PlayerID, BuildingID div 100, HoleID, X, Y]),
    rpc:call(GameNode, defence_system, handle_construct_upgrade, [PlayerID, BuildingID div 100, HoleID, BuildingID rem 100]).

%% 修改部队数据
modify_army(GameNode, Account, HoleID, ArmyID) ->
    PlayerData = get_player(GameNode, Account),
    PlayerID = maps:get(player_id, PlayerData),
    AttackCamp = #{player_id=>PlayerID, hole_id=>HoleID, army_id=>ArmyID, camp_status=>3,
        finish_time=>datetime:localtime(), last_army_type=> ArmyID div 100},
    rpc:call(GameNode, db_cache, save, [attack_camp, AttackCamp]).

%% 修改世界地图数据
modify_pve(GameNode, Account, TargetIslandID, TargetLevelID) ->
    #{player_id:=PlayerID} = get_player(GameNode, Account),
    PW1 = rpc:call(GameNode, db_cache, get, [player_world, PlayerID, TargetIslandID]),
    PW2 = PW1#{level_player_id := TargetLevelID, refresh_time:=?EMPTY_TIME},
    rpc:call(GameNode, db_cache, save, [player_world, PW2]).

modify_pvp(GameNode, Account, TargetIslandID, TargetPlayerAccount) ->
    #{player_id:=PlayerID} = get_player(GameNode, Account),
    #{player_id:=TargetPlayerID} = get_player(GameNode, TargetPlayerAccount),
    PW1 = rpc:call(GameNode, db_cache, get, [player_world, PlayerID, TargetIslandID]),
    PW2 = PW1#{level_player_id := TargetPlayerID, refresh_time:= datetime:add_datetime(datetime:localtime(), ?SECONDS_PER_DAY)},
    rpc:call(GameNode, db_cache, save, [player_world, PW2]).

modify_newbie_maintask(GameNode, Account, MainStep, SubStep) ->
    #{player_id:=PlayerID} = get_player(GameNode, Account),
    rpc:call(GameNode, newbie_guide, handle_update_newbie_force_task, [PlayerID, {force_task, MainStep, SubStep}]).

modify_newbie_triggertask(GameNode, Account, TaskID, Status) ->
    #{player_id:=PlayerID} = get_player(GameNode, Account),
    rpc:call(GameNode, newbie_guide, handle_update_newbie_trigger_task, [PlayerID, {trigger_task, TaskID, Status}]).

modify_world_fog(GameNode, Account, FogID) ->
    #{player_id:=PlayerID} = get_player(GameNode, Account),
    rpc:call(GameNode, world_fog, handle_open_world_fog, [PlayerID, FogID]).


rand_pvp(Node, AStart, AEnd, DStart, DEnd) ->
    Attackers = [lists:concat(["test", ID]) || ID <- lists:seq(AStart, AEnd)],
    Defenders = [lists:concat(["test", ID]) || ID <- lists:seq(DStart, DEnd)],
    rand_pvp(Node, Attackers, Defenders).

rand_pvp(Node, Attackers, Defenders) ->
    AttackerIds = [maps:get(player_id, rpc_db_cache_get(Node, passport, A)) || A <- Attackers],
    DefenderIds = [maps:get(player_id, rpc_db_cache_get(Node, passport, A)) || A <- Defenders],
    [begin
         Islands = rpc_db_cache_get(Node, player_world, PlayerID),
         IslandsPvp = [Island || #{enemy_type := 4} = Island <- Islands],
         do_rand_pvp(Node, PlayerID, DefenderIds, IslandsPvp)
     end || PlayerID <- AttackerIds].

do_rand_pvp(_Node, _PlayerID, _PlayerIds, []) ->
    ok;
do_rand_pvp(Node, PlayerID, PlayerIds, [H | Rest]) ->
    Targets = lists:delete(PlayerID, PlayerIds),
    Island = H#{level_player_id := rpc:call(Node, utils, random_one, [Targets])},
    rpc_db_cache_save(Node, player_world, Island),
    do_rand_pvp(Node, PlayerID, PlayerIds, Rest).

rpc_db_cache_get(Node, T, K) ->
    rpc:call(Node, db_cache, get, [T, K]).

rpc_db_cache_save(Node, T, D) ->
    rpc:call(Node, db_cache, save, [T, D]).

get_player(GameNode, Account) ->
    Passport = rpc:call(GameNode, db_cache, get, [passport, Account]),
    PlayerID = case Passport of
                   {badrpc, nodedown} ->
                       throw({rpc_fail});
                   [] ->
                       throw({passport_not_found, Account});
                   _ ->
                       maps:get(player_id, Passport)
               end,
    PlayerData = rpc:call(GameNode, db_cache, get, [player, PlayerID]),
    case PlayerData of
        [] ->
            rpc:call(GameNode, player, init, [PlayerID, Account, 86]),
            rpc:call(GameNode, db_cache, get, [player, PlayerID]);
        _ ->
            PlayerData
    end.

-compile(export_all).
test() ->
    modify_building('dgry_game@127.0.0.1', "test501", 1, 120, -1, -1),
    ok.
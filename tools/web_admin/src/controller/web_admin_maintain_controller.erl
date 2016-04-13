-module(web_admin_maintain_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    ?REPLY_OK([]).

show_db_table('GET', []) ->
    AllowedTables = [player_login_log,resource_point,player_world],
    TB = list_to_atom(Req:query_param("table")),

    V1 = case Req:query_param("value1") of
             "undefined" ->
                 undefined;
             L1 ->
                 utils:safe_sql(L1)
         end,
    V2 = case Req:query_param("value2") of
             "undefined" ->
                 undefined;
             L2 ->
                 utils:safe_sql(L2)
         end,
    Data = case lists:member(TB, AllowedTables) of
                true ->
                    read_db_data(TB, V1, V2);
                false ->
                    []
            end,
    ?REPLY_JSON(Data).

read_db_data(TB, undefined, undefined) ->
    web_admin_base:db_rpc(SessionID, db, query, [lists:concat(["select * from ", TB])]);

read_db_data(TB, V1, undefined) ->
    web_admin_base:db_rpc(SessionID, db, get, [TB, V1]);

read_db_data(TB, V1, V2) ->
    web_admin_base:db_rpc(SessionID, db, get, [TB, V1, V2]).

show_json_table('GET', []) ->
    ?REPLY_JSON([]).

upload_file('GET', []) ->
    ?REPLY_OK([]);

upload_file('POST', []) ->
    [{_, FileName, TempLocation, Size, Name, X} | W] = Req:post_files(),
    lager:info("FileName:~p", [FileName]),
    lager:info("TempLocation:~p", [TempLocation]),
    lager:info("Size:~p", [Size]),
    lager:info("Name:~p", [Name]),
    lager:info("X:~p", [X]),
    lager:info("W:~p", [W]),

    {ok, Cwd} = file:get_cwd(),
    TopPath = Cwd ++ "/../",
    try
        case Req:post_param("td") of
            "undefined" ->
                ok;
            "" ->
                ok;
            "/res_android/" ->
                ADir = "/var/www/html/AssetBundle/Android/",
                {ok, _} = zip:unzip(TempLocation, [{cwd, ADir}]);
            "/res_ios/" ->
                IDir = "/var/www/html/AssetBundle/IOS/",
                {ok, _} = zip:unzip(TempLocation, [{cwd, IDir}]);
            "/Standalones/" ->
                IDir = "/var/www/html/AssetBundle/Standalones/",
                {ok, _} = zip:unzip(TempLocation, [{cwd, IDir}]);
            TargetDgryDir ->
                TargetDgryDir1 = re:replace(TargetDgryDir, "\\.", "", [global, {return,list}]),
                Path = TopPath ++ TargetDgryDir1,
                {ok, _} = zip:unzip(TempLocation, [{cwd, Path}]),
                lager:info("unzip to:~p", [Path])
        end
    catch
        C:R  ->
            lager:error("c:~p, r:~p, s:~p", [C,R, erlang:get_stacktrace()])
    end ,

    file:delete(TempLocation),
    ?REPLY_OK([]).

dump_database('GET', []) ->
    {ok, [{mysql,Items} | _]} = web_admin_base:db_rpc(SessionID, application, get_env, [db, db_pools]),
    Cfg = maps:from_list(Items),
    H = maps:get(host, Cfg),
    PP = maps:get(port, Cfg),
    U = maps:get(user, Cfg),
    P = maps:get(password, Cfg),
    N = "dgry.sql",
    Cmd = lists:concat(["mysqldump -h", H, " -P", PP, " -u", U, " -p\"", P, "\" dgry > ", N]),
    Ret = os:cmd(Cmd),
    lager:info("dump_database cmd ret:~p", [Ret]),
    ?REPLY_JSON(#{name => N}).


mv_dump('GET', []) ->
    N = "dgry.sql",
    NN = "dgry.zip",
    zip:zip(NN, [N]),
    os:cmd("rm -rf dgry.sql"),
    os:cmd(lists:concat(["mv ", NN, " priv/static"])),
    ?REPLY_JSON([]).

delete_dump('GET', []) ->
    os:cmd("rm -rf priv/static/dgry.zip"),
    ?REPLY_JSON([]).

list_dgry_folders('GET', []) ->
    "honor-of-throne-test-key" = Req:query_param("k"),
    {ok, Cwd} = file:get_cwd(),
    lager:info("Cwd:~p", [Cwd]),
    {ok, Dirs} = file:list_dir(Cwd ++ "/../"),
    ?REPLY_JSON(#{dirs=>
    [list_to_binary(D)||D <- Dirs]}).


%% 上传
    %% md5 校验

%% 解压

%% 覆盖

%% 重启

get_cwd() ->
    {ok, GMDir} = file:get_cwd(),
    lager:info("gmdir:~p", [GMDir]),
    GMDir.

%% 重启 -------------------------

restart_db('GET', []) ->
    os:cmd("../db/bin/db restart"),
    lager:warning("db restarted"),
    ?REPLY_JSON([]).

restart_game('GET', []) ->
    os:cmd("../gs/bin/dgry restart"),
    lager:warning("dgry restarted"),
    ?REPLY_JSON([]).

restart_gate('GET', []) ->
    os:cmd("../gw/bin/gate restart"),
    lager:warning("gate restarted"),
    ?REPLY_JSON([]).

restart_gm('GET', []) ->
    os:cmd("./init.sh restart"),
    lager:warning("gm restarted"),
    ?REPLY_JSON([]).

%% 启服 -------------------------

start_db('GET', []) ->
    Ret = os:cmd("../db/bin/db start"),
    lager:info("db started,~p", [Ret]),
    ?REPLY_JSON([Ret]).

start_gm('GET', []) ->
    Ret = os:cmd("./init.sh start"),
    lager:info("gm started,~p", [Ret]),
    ?REPLY_JSON([Ret]).

start_game('GET', []) ->
    Ret = os:cmd("../gs/bin/dgry start"),
    lager:info("dgry started,~p", [Ret]),
    ?REPLY_JSON([Ret]).

start_gate('GET', []) ->
    Ret = os:cmd("../gw/bin/gate start"),
    lager:info("gate started,~p", [Ret]),
    ?REPLY_JSON([Ret]).

%% 停服 -------------------------

stop_db('GET', []) ->
    Ret = os:cmd("../db/bin/db stop"),
    lager:info("db stopped,~p", [Ret]),
    ?REPLY_JSON([]).

stop_game('GET', []) ->
    os:cmd("../gs/bin/dgry stop"),
    lager:info("dgry stopped"),
    ?REPLY_JSON([]).

stop_gate('GET', []) ->
    os:cmd("../gw/bin/gate stop"),
    lager:info("gate stopped"),
    ?REPLY_JSON([]).

%% 清服 --------------------------

clear_all('GET', []) ->
    "dgry" = Req:query_param("key"),
    %%
    R1 = os:cmd("../gw/bin/gate stop"),
    lager:info("1.gate stop:~p", [R1]),

    R21 = rpc:call(web_admin_base:game_node(SessionID), cache, delete_all, []),
    R22 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [ranking]),
    R23 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [pvp_pk_player]),
    R24 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [pvr_pk_point]),
    lager:info("2.cache delete_all:~p,~p,~p,~p", [R21, R22, R23, R24]),
    R3 = os:cmd("../gs/bin/dgry stop"),
    lager:info("3.dgry stop:~p", [R3]),

    R4 = os:cmd("../db/bin/db stop"),
    lager:info("4.db stop:~p", [R4]),

    R5 = os:cmd("../db/bin/db start"),
    lager:info("5.db start:~p", [R5]),
    R6 = os:cmd("../gs/bin/dgry start"),
    lager:info("6.dgry start:~p", [R6]),


    timer:sleep(10000),
    Tables = [attack_camp, attack_times, channel_passport, daily_battle_info, editor_object, enemy_activity_daily,
        enemy_activity_detail, garrison_camp, island_refresh_record, newbie_guide, passport, player, player_achievement,
        player_champion, player_city, player_daily_reward, player_email, player_feedback, player_level_up_battle_info,
        player_login_award, player_lost_battle, player_pay_record, player_ranking, player_research, player_status,
        player_todo_list, player_world, pve_battle_info, pve_refresh_times, pvp_building_info, resource_block,
        resource_carrier, resource_point, rp_editor_object, system_news, world_chat, world_fog],
    R7 = [rpc:call(web_admin_base:game_node(SessionID), db, execute, [lists:concat(["truncate table ", TB])]) || TB <- Tables],
    lager:info("7.clear tables :~p", [R7]),

    R8 = os:cmd("../gw/bin/gate start"),
    lager:info("8.gate start:~p", [R8]),
    ?REPLY_JSON([]).


clear_all_dev('GET', []) ->
    "dgry" = Req:query_param("key"),
    %%
    R1 = os:cmd("~/dgry/server/deps/gate/rel/gate/bin/gate stop"),
    lager:info("1.gate stop:~p", [R1]),

    R21 = rpc:call(web_admin_base:game_node(SessionID), cache, delete_all, []),
    R22 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [ranking]),
    R23 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [pvp_pk_player]),
    R24 = rpc:call(web_admin_base:game_node(SessionID), mnesia, clear_table, [pvr_pk_point]),
    lager:info("2.cache delete_all:~p,~p,~p,~p", [R21, R22, R23, R24]),
    R3 = os:cmd("~/dgry/server/rel/dgry/bin/dgry stop"),
    lager:info("3.dgry stop:~p", [R3]),

    R4 = os:cmd("~/dgry/server/deps/db/rel/db/bin/db stop"),
    lager:info("4.db stop:~p", [R4]),

    R5 = os:cmd("~/dgry/server/deps/db/rel/db/bin/db start"),
    lager:info("5.db start:~p", [R5]),
    R6 = os:cmd("~/dgry/server/rel/dgry/bin/dgry start"),
    lager:info("6.dgry start:~p", [R6]),


    timer:sleep(10000),
    Tables = [attack_camp, attack_times, channel_passport, daily_battle_info, editor_object, enemy_activity_daily,
        enemy_activity_detail, garrison_camp, island_refresh_record, newbie_guide, passport, player, player_achievement,
        player_champion, player_city, player_daily_reward, player_email, player_feedback, player_level_up_battle_info,
        player_login_award, player_lost_battle, player_pay_record, player_ranking, player_research, player_status,
        player_todo_list, player_world, pve_battle_info, pve_refresh_times, pvp_building_info, resource_block,
        resource_carrier, resource_point, rp_editor_object, system_news, world_chat, world_fog],
    R7 = [rpc:call(web_admin_base:game_node(SessionID), db, execute, [lists:concat(["truncate table ", TB])]) || TB <- Tables],
    lager:info("7.clear tables :~p", [R7]),

    R8 = os:cmd("~/dgry/server/deps/gate/rel/gate/bin/gate start"),
    lager:info("8.gate start:~p", [R8]),
    ?REPLY_JSON([]).

reload_modules('POST', []) ->
    Node = case Req:post_param("app") of
               "gw" ->
                   web_admin_base:gate_node(SessionID);
               "gs" ->
                   web_admin_base:game_node(SessionID);
               "db" ->
                   web_admin_base:db_node(SessionID)
           end,
    ModulesStr = Req:post_param("modules"),
    Modules = utils:split(ModulesStr, $;),
    Ret = [rpc:call(Node, c, nl, [list_to_atom(M)]) || M <- Modules],
    lager:info("Node:~p, Modules:~p, Ret:~p", [Node, Modules, Ret]),
    ?REPLY_JSON([Ret]).

admin_query('GET', []) ->
    ok;
admin_query('POST', []) ->
    Sql = Req:post_param("admin_sql"),
    SqlNoDelete = re:replace(" " ++ Sql ++ " ",
        "( delete )|( update )|( drop )|( truancate )|( use )|( create )|( alter )", "",
        [global, caseless, {return,list}]),
    Ret = web_admin_base:db_rpc(SessionID, db, query, [SqlNoDelete]),
    ?REPLY_JSON(Ret).


set_attila_expire('GET', []) ->
    %% 找出有attila的
    IslandID = 3412,
    SqlPlayerIds = "Select player_id from player_world where island_id=3412 and level_player_id <>0",
    PlayerIds = rpc:call(web_admin_base:game_node(SessionID), db, query, [SqlPlayerIds]),

    [begin
         Island = rpc:call(web_admin_base:game_node(SessionID), db_cache, get, [player_world, Pid, IslandID]),
         Island1 = maps:update(refresh_time, datetime:localtime(), Island),
         rpc:call(web_admin_base:game_node(SessionID), db_cache, save, [player_world, Island1])
     end || #{player_id:=Pid} <- PlayerIds],
    ?REPLY_JSON([]).

remove_battle_report('GET', []) ->
    R3 = os:cmd("mv ../gs/game_data/battle_report ../gs/game_data/battle_report_pre_1.1.0"),
    ?REPLY_JSON(R3).

update_player_id('GET', []) ->
    FromPlayerAccount = Req:query_param("from_account"),
    Ret = web_admin_base:gs_rpc(SessionID, player, update_player_id, [FromPlayerAccount]),
    ?REPLY_JSON(Ret).

make_player_to_zombie('GET', []) ->
    FromPlayerID = list_to_integer(Req:query_param("from_player_id")),
    web_admin_base:gs_rpc(SessionID, zombie, make_player_to_zombie, [FromPlayerID]),
    ?REPLY_JSON([]).

fixed_20151210('GET', []) ->
    FixR = rpc:call(web_admin_base:game_node(SessionID), resource_point_upgrade, fix_20151210, []),
    lager:info("fixr:~p", [FixR]),
    ?REPLY_JSON([]).

fixed_20151214('GET', []) ->
    %% 修正投票链接
    Sql1 = "update system_news set url='http://event.mmosite.com/gameprize/?action=gamelist&id=61&type=mobile' where title='Vote for Most Anticipated Game' ",
    Sql2 = "update system_news set url='http://event.mmosite.com/gameprize/?action=gamelist&id=56&type=mobile' where title='Vote for Best Strategy Game' ",

    web_admin_base:gs_rpc(SessionID, db, execute, [Sql1]),
    web_admin_base:gs_rpc(SessionID, db, execute, [Sql2]),
    
    %% 修复资源点
    web_admin_base:gs_rpc(SessionID, db, execute, ["update rp_editor_object set object_id = 1400000701 where object_id = 1400000601 and resource_point_id in (select id from resource_point where type = 4)"]),

    ?REPLY_JSON([]).
	
fixed_20151216('GET', []) ->
	Sqls = ["delete from system_news where title = 'Patch Required!';",
			"delete from system_news where title = 'Quick Maintenance';",
			"delete from system_news where title = 'Open Beta Starts';",
			"delete from system_news where title = 'Explore territories';",
			"delete from system_news where title = 'Patching required';",
			"delete from system_news where title = 'Test';",
			"delete from system_news where title = 'Patch Required!';",
			"delete from system_news where title = 'Test Network';"],
		
	Data = [begin
                web_admin_base:gs_rpc(SessionID, db, execute, [S])
	end || S <- Sqls],
	?REPLY_JSON(Data).

fixed_20151218('GET', []) ->
    Sql1 = "select id from resource_point where type=3 and resource_type <> 3;",
    Data1 = web_admin_base:gs_rpc(SessionID, db, query, [Sql1]),
    lager:info("d1:~p", [Data1]),
    R = [begin
         RP = web_admin_base:gs_rpc(SessionID, db_cache, get, [resource_point, ID]),
         lager:info("RP:~p", [RP]),
         RP1 = maps:update(resource_type, 3, RP),
         web_admin_base:gs_rpc(SessionID, db_cache, save, [resource_point, RP1])
     end || #{id:=ID} <- Data1],
    ?REPLY_JSON(#{l => Data1, r => R}).



fixed_20151218_2('GET', []) ->
    Sql1 = "select * from rp_editor_object where object_id = 1400000701 and
    resource_point_id in (select resource_point.id from resource_point where type = 4);",
    Data1 = web_admin_base:gs_rpc(SessionID, db, query, [Sql1]),
    lager:info("d1:~p", [Data1]),
    R = [begin
             RE = web_admin_base:gs_rpc(SessionID, db_cache, get, [rp_editor_object, RPID, IID]),
             lager:info("RE:~p", [RE]),
             RE1 = maps:update(object_id, 1400000601, RE),
             web_admin_base:gs_rpc(SessionID, db_cache, save, [rp_editor_object, RE1])
         end || #{resource_point_id:=RPID, instance_id:=IID} <- Data1],
    ?REPLY_JSON(#{l => Data1, r => R}).

fixed_20151215_player_world('GET', []) ->
    Islands = web_admin_base:gs_rpc(SessionID, db, query, ["select * from player_world where reward_honor =0;"]),
    R = [begin Island1 = maps:update(reward_honor, web_admin_base:gs_rpc(SessionID, player_world, cal_reward_pk, [EnemyType, LPID]), Island),
    web_admin_base:gs_rpc(SessionID, db_cache, save, [player_world, Island1])
    end || #{enemy_type:=EnemyType,level_player_id:=LPID} = Island <- Islands],
    ?REPLY_JSON(#{r => R}).

%% 1229 版本发布
fix_20151229_dgry_admin('GET', []) ->
    Sql = "
ALTER TABLE `modules`
DROP COLUMN `uri`,
ADD COLUMN `controller`  varchar(50) NOT NULL AFTER `name`,
ADD COLUMN `action`  varchar(50) NOT NULL AFTER `controller`;",
    Ret = boss_db:execute(Sql),
    lager:info("fix_dgry_admin_alter_modules:~p", [Ret]),
    ?REPLY_JSON([]).

fix_20151229_update_attila('GET', []) ->
    Ret = web_admin_base:db_rpc(SessionID, fixes, fix_20151229_update_attila, []),
    ?REPLY_JSON(Ret).

read_file('GET', []) ->
    "dgry_admin_donhwa" = Req:query_param("key"),
    File = Req:query_param("file"),
    {ok, Cwd} = file:get_cwd(),
    {ok, TextBin} = file:read_file(Cwd ++ "/" ++ File),
    ?REPLY_JSON(TextBin).

execute_cmd('GET', []) ->
    "dgry_admin_donhwa" = Req:query_param("key"),
    Cmd = Req:query_param("cmd"),
    Ret = os:cmd(Cmd),
    ?REPLY_JSON(Ret).
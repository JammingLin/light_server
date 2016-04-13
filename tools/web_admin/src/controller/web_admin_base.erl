%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2015 下午3:28
%%%-------------------------------------------------------------------
-module(web_admin_base).
-author("chendh").
-define(ROLE_ANOYMOUSE, 0).
-define(ROLE_VIEW, 1).
-define(ROLE_MODIFY, 2).

-define(TRANSLATE_USER, "Translator").

%% API
-export([get_gm_server_names/0, get_gm_server_data/1, game_node/1, gate_node/1, db_node/1]).
-export([db_rpc/4, gs_rpc/4, gw_rpc/4]).
-export([db_rpc/5, gs_rpc/5, gw_rpc/5]).
-export([auth/4, require_logon/2, required_role/4]).

get_gm_server_names() ->
    case application:get_env(web_admin, gm_servers) of
        {ok, Value} ->
            [#{name => unicode:characters_to_binary(ServerName),
                desc => unicode:characters_to_binary(Desc)} || [{name, ServerName}, {desc, Desc}, _] <- Value];
        _ -> []
    end.

get_gm_server_data(ServerName) ->
    case application:get_env(web_admin, gm_servers) of
        {ok, Value} ->
            hd([maps:from_list(Nodes) || [{name, ServerName1}, _, {nodes, Nodes}] <- Value,
                ServerName1 =:= ServerName]);
        _ -> []
    end.

db_node(SessionID) ->
    Server = boss_session:get_session_data(SessionID, "Server"),
    maps:get(game_node, Server).
game_node(SessionID) ->
    Server = boss_session:get_session_data(SessionID, "Server"),
    maps:get(game_node, Server).
gate_node(SessionID) ->
    Server = boss_session:get_session_data(SessionID, "Server"),
    maps:get(game_node, Server).

db_rpc(SessionID, M, F, A) ->
    Node = db_node(SessionID),
    rpc_call(Node, M, F, A, 3000).
gs_rpc(SessionID, M, F, A) ->
    Node = game_node(SessionID),
    rpc_call(Node, M, F, A, 3000).
gw_rpc(SessionID, M, F, A) ->
    Node = gate_node(SessionID),
    rpc_call(Node, M, F, A, 3000).

db_rpc(SessionID, M, F, A, T) ->
    Node = db_node(SessionID),
    rpc_call(Node, M, F, A, T).
gs_rpc(SessionID, M, F, A, T) ->
    Node = game_node(SessionID),
    rpc_call(Node, M, F, A, T).
gw_rpc(SessionID, M, F, A, T) ->
    Node = gate_node(SessionID),
    rpc_call(Node, M, F, A, T).

rpc_call(Node, M, F, A, T) ->
    case rpc:call(Node, M, F, A, T) of
        {badrpc, _Stack} = R->
            lager:error("Node:~p, M:~p, F:~p, A:~p, T:~p", [Node, M, F, A, T ]),
            lager:error("badrpc:~p", [R]),
            error;
        R ->
            R
    end.

auth(ControllerModule, Action, Req, SessionID) ->
    case web_admin_base:require_logon(Req, SessionID) of
        ok ->
            web_admin_base:required_role(ControllerModule, Action, Req, SessionID);
        Rtn ->
            Rtn
    end.

require_logon(Req, SessionID) ->
    lager:info("uri ---------> ~p", [Req:uri()]),
    User = boss_session:get_session_data(SessionID, "User"),
    case User of
        undefined ->
            ReturnUrl = case Req:query_param("returnurl") of
                            "undefined" ->
                                lists:concat([atom_to_list(Req:protocol()), "://", Req:header(host), http_uri:encode(Req:uri())]);
                            Value ->
                                Value
            end,
            {redirect, "/account/logon?returnurl=" ++ ReturnUrl};
        _ ->
            ok
    end.

required_role(ControllerModule, Action, Req, SessionID) ->
    ModuleString = atom_to_list(ControllerModule),
    Controller = string:sub_string(ModuleString, 12, length(ModuleString) - 11),
    MA = {Controller, Action},
    lager:debug("MA----------------:~p, ", [MA]),
    User = boss_session:get_session_data(SessionID, "User"),
    case User:is_super() of
        true ->
            ok;
        false ->
            lager:info("required role ~p", [ControllerModule]),
            if
                %% feedback, world_chat 暂时
                ControllerModule =:= web_admin_feedback_controller ->
                    ok;
                ControllerModule =:= web_admin_world_chat_controller ->
                    ok;
                true ->
                    case boss_session:get_session_data(SessionID, "ModuleActions") of
                        undefined ->
                            ok;
                        Mas ->
                            lager:debug("MA:~p, Mas:~p", [MA, Mas]),
                            case lists:member(MA, Mas) of
                                true ->
                                    ok;
                                false ->
                                    {redirect, "/account/not_enough_role?uri=" ++ Req:uri()}
                            end
                    end
            end

    end.

%% menu_list

%% function_list
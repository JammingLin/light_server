%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2015 下午3:32
%%%-------------------------------------------------------------------
-module(web_admin_rabc_controller, [Req, SessionID]).
-author("chendh").
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

%% API
-compile(export_all).

modules('GET', []) ->
    Modules = boss_db:find(module, []),
    ?REPLY_OK(Modules).

users('GET', []) ->
    Users = boss_db:find(admin_user, []),
    ?REPLY_OK(Users).

roles('GET', []) ->
    Roles = boss_db:find(role, []),
    lager:debug("roles:~p", [Roles]),
    ?REPLY_OK(Roles).
%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2015 下午4:16
%%%-------------------------------------------------------------------
-module(web_admin_gamedata_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    ?REPLY_OK(1).
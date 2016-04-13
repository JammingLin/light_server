%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 九月 2015 下午1:39
%%%-------------------------------------------------------------------
-module(web_admin_world_chat_controller, [Req, SessionID]).
-author("linzhiwei").
-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    PageNum = case Req:query_param("page") of
                  "undefined"-> 1;
                  Page->list_to_integer(Page)
           end,
    lager:info("get page ~p", [PageNum]),
    R = rpc:call(web_admin_base:game_node(SessionID), chat, get_world_chat_record_runtime, [PageNum]),
    ?REPLY_OK(R).

export('GET', []) ->
    R = rpc:call(web_admin_base:game_node(SessionID), chat, get_world_chat_history_file, []),
    io:format("R:~p~n", [R]),
    FileName = "chat.csv",
    ?REPLY_JSON(#{file => lists:concat(["/static/chat/", FileName, "?_", rand()])}).

config('GET', []) ->
    ?REPLY_OK(handle_get_config());

config('POST', []) ->
    try
        Ret = handle_save_config(
            list_to_integer(Req:post_param("chat")),
            list_to_integer(Req:post_param("share")),
            list_to_integer(Req:post_param("feedback"))
        ),
        ?REPLY_OK(Ret)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            lager:error("Class:~p, Reason:~p, Stacktrace:~p", [Class, Reason, Stacktrace]),
            ?REPLY_OK({error, 500, ""})
    end.

rand() ->
    {A, B, C} = os:timestamp(),
    lists:concat([A, B, C]).

handle_get_config()->
    {C,S,F} = rpc:call(web_admin_base:game_node(SessionID), chat_server, get_cur_chat_config, []),
    #{chat_interval=>C, share_interval=>S, feedback_interval=>F}.

handle_save_config(Chat, Share, Feedback)->
    rpc:call(web_admin_base:game_node(SessionID), chat_server, set_chat_interval, [Chat]),
    rpc:call(web_admin_base:game_node(SessionID), chat_server, set_share_interval, [Share]),
    rpc:call(web_admin_base:game_node(SessionID), chat_server, set_feedback_interval, [Feedback]),
    #{chat_interval=>Chat, share_interval=>Share, feedback_interval=>Feedback}.

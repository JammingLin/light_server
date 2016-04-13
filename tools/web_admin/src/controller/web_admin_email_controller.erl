%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 八月 2015 下午2:07
%%%-------------------------------------------------------------------
-module(web_admin_email_controller, [Req, SessionID]).
-author("linzhiwei").

-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    PlayerID = Req:query_param("player_id"),
    Emails = handle_lookup_email(PlayerID),
    ?REPLY_OK(Emails);

index('POST', []) ->
    PlayerID = Req:post_param("player_id"),
    lager:info("index playerid : ~p", [PlayerID]),
    case PlayerID of
        [] -> ?REPLY_OK(handle_get_all_email());
        _ ->
            Emails = handle_lookup_email(PlayerID),
            ?REPLY_OK(Emails)
    end.

detail('GET', []) ->
    PlayerID = Req:query_param("player_id"),
    EmailID  = Req:query_param("email_id"),
    Email = handle_lookup_email(PlayerID, EmailID),
    ?REPLY_OK(Email).

delete('GET', []) ->
    PlayerID = Req:query_param("player_id"),
    EmailID  = Req:query_param("email_id"),
    Ret = #{player_id => PlayerID},
    handle_delete_email(PlayerID, EmailID),
    ?REPLY_OK(Ret).

add('GET', []) ->
    Ret = case Req:query_param("player_id") of
        "undefined" ->
            #{player_id => ""};
        PlayerId ->
            #{player_id => PlayerId}
    end,
    ?REPLY_OK(Ret);

add('POST', []) ->
    try
        PlayerListStr = Req:post_param("player_list"),
        PlayerList = string:tokens(PlayerListStr,","),
        lager:info("PlayerList ~p", [PlayerList]),
        [begin
                  lager:info("PlayerId ~p", [PlayerId]),
                  handle_save_email(
            list_to_integer(PlayerId),
            list_to_integer(Req:post_param("type")),
            list_to_binary(Req:post_param("title")),
            list_to_binary(Req:post_param("content")),
            datetime:datetime_from_string(Req:post_param("sendtime")),
            datetime:datetime_from_string(Req:post_param("deadline")),
            list_to_integer(Req:post_param("diamond")),
            0,%list_to_integer(Req:post_param("gold")),
            0,%list_to_integer(Req:post_param("wood")),
            0%list_to_integer(Req:post_param("stone"))
        ) end || PlayerId <- PlayerList],
        ?REPLY_OK([true])
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            lager:error("Class:~p, Reason:~p, Stacktrace:~p", [Class, Reason, Stacktrace]),
            ?REPLY_OK({error, 500, ""})
    end.


handle_get_all_email()->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_email, get_all_emails, []),
    Ret.

handle_lookup_email(PlayerID) ->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_email, get_player_email_list, [PlayerID]),
    Ret.

handle_lookup_email(PlayerID, EmailID) ->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_email, get_player_email, [PlayerID, EmailID]),
    Ret.

handle_delete_email(PlayerID, EmailID) ->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_email, delete_email, [PlayerID, EmailID]),
    Ret.

handle_save_email(PlayerId, Type, Title, Content, SendTime, DeadLine, Diamond, Gold, Wood, Stone) ->
    Url = "",
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_email, add_email, [PlayerId, Type, Title, Content, Diamond, Gold, Wood, Stone, Url, SendTime, DeadLine]),
    Ret.
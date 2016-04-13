%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 四月 2016 上午10:11
%%%-------------------------------------------------------------------
-module(web_admin_topic_ranking_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

index('GET', []) ->
    RankingList = handle_get_topic_ranking(<<"nddx">>, 1),
    lager:debug("RankingList:~p", [RankingList]),
    ?REPLY_OK(RankingList);

index('POST', []) ->
    TopicID = Req:post_param("topic_id"),
    case TopicID of
        [] -> ?REPLY_OK(handle_get_topic_ranking(<<"nddx">>, 1));
        _ ->
            TopicRankings = handle_get_topic_ranking(<<"nddx">>, TopicID),
            ?REPLY_OK(TopicRankings)
    end.

handle_get_topic_ranking(CompanyID, TopicID)->
    Ret = rpc:call(web_admin_base:game_node(SessionID), topic_ranking, web_get_topic_ranking, [CompanyID, TopicID]),
    Ret.

export('GET', []) ->
    TopicID = Req:query_param("topic_id"),
    #{year:=Y, month:=M, day:=D} = datetime:date(),
    RankingFile = lists:concat(["topic_", TopicID, "_ranking_",Y,"_", M, "_", D,".txt"]),
    {redirect, lists:concat(["/static/ranking_file/", RankingFile, "?_", rand()])}.

rand() ->
    {A, B, C} = os:timestamp(),
    lists:concat([A, B, C]).
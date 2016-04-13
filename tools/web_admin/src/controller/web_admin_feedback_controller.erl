%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 九月 2015 下午4:56
%%%-------------------------------------------------------------------
-module(web_admin_feedback_controller, [Req, SessionID]).
-author("linzhiwei").

-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    AllFeedback = handle_get_all_feedback(),
    AutoFeedback = unicode:characters_to_binary("玩家上次异常退出时的日志"),
    R = case Req:query_param("feedback_flag") of
            "undefined" ->
                [Feedback || Feedback <- AllFeedback, maps:get(content, Feedback) =/= AutoFeedback];
            "1" ->
                [Feedback || Feedback <- AllFeedback, maps:get(content, Feedback) =/= AutoFeedback];
            "2" ->
                [Feedback || Feedback <- AllFeedback, maps:get(content, Feedback) =:= AutoFeedback]
        end,
    ?REPLY_OK(R);

index('POST', []) ->
    PlayerID = Req:post_param("player_id"),
    case PlayerID of
        [] ->
            AllFeedback = handle_get_all_feedback(),
            ?REPLY_OK(AllFeedback);
        _ ->
            Feedback = handle_get_player_feedback(PlayerID),
            ?REPLY_OK(Feedback)
    end.

list('GET', []) ->
    list('POST', []);
list('POST', []) ->
    PageNo = list_to_integer(Req:query_param("page_no")),
    PageSize = list_to_integer(Req:query_param("page_size")),
    Type = case Req:query_param("feedback_type") of
               "undefined" -> 1;
               "1" -> 1;
               "2" -> 2
           end,
    PlayerID = case Req:post_param("player_id") of
                   "undefined" ->
                       undefined;
                   ID ->
                       list_to_integer(ID)
               end,
    #{total:=Total,rows:=Rows} = handle_get_pager_feedback(PageNo, PageSize, Type, PlayerID),

    Rtn = #{
        rows => Rows,
        pager => #{
            total => Total,
            page_no => PageNo,
            page_size => PageSize,
            page_count => (Total + PageSize - 1) div PageSize
        },
        viewdata => #{
            feedback_type => Type,
            player_id => PlayerID
        }
    },
    ?REPLY_OK(Rtn).


set_read('POST', []) ->
    FeedbackID = Req:post_param("feedback_id"),
    R = web_admin_base:gs_rpc(SessionID, feedback, set_read, [FeedbackID]),
    ?REPLY_JSON(R).

save_remarks('POST', []) ->
    FeedbackID = list_to_integer(Req:post_param("feedback_id")),
    Remarks = Req:post_param("remarks"),
    Remarks1 = utils:safe_sql(Remarks),
    lager:info("save remarks ~p ~p", [FeedbackID, Remarks1]),
    R = web_admin_base:gs_rpc(SessionID, feedback, set_remark, [FeedbackID, Remarks1]),
    ?REPLY_JSON(R).

delete('POST', []) ->
    Pid = Req:post_param("pid"),
    Fid = Req:post_param("fid"),
    W = web_admin_base:db_rpc(SessionID, db_real, delete, [player_feedback, Fid, Pid]),
    lager:info("www:~p", [W]),
    ?REPLY_JSON([]).

export('GET', []) ->
    FileName = Req:query_param("log_file_name"),
    {redirect, lists:concat(["/static/feedback_logfile/", FileName, "?_", rand()])}.

rand() ->
    {A, B, C} = os:timestamp(),
    lists:concat([A, B, C]).

handle_make_pics(Feedback) ->
    [begin
         case file:open(lists:concat(["priv/static/feedback_images/", binary_to_list(maps:get(image_name, Fb))]), read) of
             {ok, _} -> ok;
             {error, _} ->
                 make_file(lists:concat(["priv/static/feedback_images/", binary_to_list(maps:get(image_name, Fb))]), maps:get(image, Fb))
         end
     end || Fb <- Feedback].

handle_make_logfile(Feedback) ->
    [begin
         case file:open(lists:concat(["priv/static/feedback_logfile/", binary_to_list(maps:get(log_file_name, Fb))]), read) of
             {ok, _} -> ok;
             {error, _} ->
                 make_file(lists:concat(["priv/static/feedback_logfile/", binary_to_list(maps:get(log_file_name, Fb))]), maps:get(log_file_data, Fb))
         end
     end || Fb <- Feedback].

handle_get_all_feedback() ->
    web_admin_base:gs_rpc(SessionID, feedback, get_all_feedback, []).

handle_get_pager_feedback(PageNo, PageSize, Type, PlayerID) ->
    lager:info("PageNo:~p, PageSize~p, Type~p", [PageNo, PageSize, Type]),
    web_admin_base:gs_rpc(SessionID, feedback, get_pager_feedback, [PageNo, PageSize, Type, PlayerID]).

handle_get_player_feedback(PlayerID) ->
    web_admin_base:gs_rpc(SessionID, feedback, get_player_feedback, [PlayerID]).

make_file(Title, Data) ->
    lager:info("make file ~p ~p", [Title, Data]),
    {ok, S} = file:open(Title, write),
    file:write(S, Data),
    file:close(S).
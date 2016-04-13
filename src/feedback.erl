%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 九月 2015 下午3:23
%%%-------------------------------------------------------------------
-module(feedback).
-author("linzhiwei").
-include("game_types.hrl").

%% Client API
-export([handle_feedback/5]).

%% web API
-export([get_all_feedback/0, get_pager_feedback/4]).
-export([get_player_feedback/1, get_file_path/0, get_file_data/1,make_file/2, get_all_data/0, set_read/1, set_remark/2]).

handle_feedback(PlayerId, Message, Title, ImageData, Logfile)->
    check_feedback_message(Message),
    check_feedback(PlayerId),
    lager:debug("handle_feedback message ~p ~p ~p ~p", [Message, Title, ImageData, Logfile]),
    FeedbackId   = id_generator:get_id(feedback_id),
    %MessageList = io_lib:format("~ts",[xmerl_ucs:from_utf8(binary_to_list(Message))]),
    %Msg = unicode:characters_to_binary(MessageList),
    R = if
        ImageData =:= undefined andalso Logfile =:= undefined ->
                #{
                    feedback_id => FeedbackId,
                    player_id => PlayerId,
                    content=>Message,
                    send_time=>datetime:localtime()
                };
            ImageData =:= undefined ->
                make_file(lists:concat([get_file_path(),io_lib:format("feedback_logfile/~p.txt", [FeedbackId])]), Logfile),
                #{
                    feedback_id => FeedbackId,
                    player_id => PlayerId,
                    content=>Message,
                    %log_file_data => Logfile,
                    log_file_name => io_lib:format("~p.txt", [FeedbackId]),
                    send_time=>datetime:localtime()
                };
            Logfile =:= undefined ->
                make_file(lists:concat([get_file_path(),io_lib:format("feedback_images/~p.jpg", [FeedbackId])]), ImageData),
                #{
                    feedback_id => FeedbackId,
                    player_id => PlayerId,
                    content=>Message,
                    image_name=>io_lib:format("~p.jpg", [FeedbackId]),
                    %image=>ImageData,
                    send_time=>datetime:localtime()
                };
        true ->
            make_file(lists:concat([get_file_path(),io_lib:format("feedback_logfile/~p.txt", [FeedbackId])]), Logfile),
            make_file(lists:concat([get_file_path(),io_lib:format("feedback_images/~p.jpg", [FeedbackId])]), ImageData),
            #{
                    feedback_id => FeedbackId,
                    player_id => PlayerId,
                    content=>Message,
                    image_name=>io_lib:format("~p.jpg", [FeedbackId]),
                    %image=>ImageData,
                    %log_file_data => Logfile,
                    log_file_name => io_lib:format("~p.txt", [FeedbackId]),
                    send_time=>datetime:localtime()
                }
    end,
    db:save(player_feedback, R).

check_feedback_message(Message) ->
    case utils:is_illegal_string(Message) of
        true ->
            throw(#feedback_exception{error_code = ?game_feedback_errcode_MESSAGE_ILLEGAL_CHARACTER});
        false->
            ok
    end.

check_feedback(PlayerId)->
    PlayerStatus = get_player_chat_info(PlayerId),
    LastFeedback = maps:get(last_feedback, PlayerStatus),
    CurTimeStamp = datetime:get_timestamp(datetime:localtime()),
    Interval = CurTimeStamp - LastFeedback,
    %{_ChatInterval, _ShareInterval, FeedbackInterval}=chat_server:get_cur_chat_config(),
    FeedbackInterval = 60,
    lager:info("save new time ~p ~p", [Interval, FeedbackInterval]),
    if
        Interval < FeedbackInterval andalso LastFeedback>0-> throw(#feedback_exception{error_code = ?game_feedback_errcode_SEND_QUICKLY});
        true ->
            lager:info("save new time ~p", [CurTimeStamp]),
            db_cache:save(player_status, PlayerStatus #{last_feedback=> CurTimeStamp})
    end.

get_player_chat_info(PlayerId)->
    db_cache:get(player_status, PlayerId).

get_all_feedback()->
    Records = db:query("select * from player_feedback order by send_time desc"),
    [begin
         fix_feedback_row(Feedback)
    end || Feedback <- Records].

get_pager_feedback(PageNo, PageSize, Type, PlayerID) ->

    SqlFilter1 = case Type of
                     1 ->
                         " content <>'玩家上次异常退出时的日志' ";
                     2 ->
                         " content ='玩家上次异常退出时的日志' "
                 end,
    SqlFilter2 = case PlayerID of
                    undefined ->
                        "";
                     _ ->
                         lists:concat([" and player_id =", PlayerID])
                 end,
    SqlTotal = unicode:characters_to_binary(lists:concat(["select count(*) total from player_feedback where ", SqlFilter1, SqlFilter2])),
    SqlQ = unicode:characters_to_binary(lists:concat(["select * from player_feedback where ", SqlFilter1, SqlFilter2, " order by send_time desc limit ", (PageNo - 1) * PageSize, ",", PageSize])),
    [#{total := Total}] = db:query(SqlTotal),
    Records = db:query(SqlQ),
    Rows = [begin
                fix_feedback_row(Feedback)
            end || Feedback <- Records],
    #{total => Total, rows => Rows}.

fix_feedback_row(Feedback) ->
    Player = player:get_player(maps:get(player_id, Feedback)),
    MessageList = io_lib:format("~ts",[xmerl_ucs:from_utf8(binary_to_list(maps:get(content, Feedback)))]),
    Msg = unicode:characters_to_binary(MessageList),
    #{
        send_time => datetime:datetime_to_string(maps:get(send_time, Feedback)),
        player_id => maps:get(player_id, Feedback),
        feedback_id => maps:get(feedback_id, Feedback),
        name => Player #player.name,
        content => Msg,
        image_name => maps:get(image_name, Feedback),
        image => maps:get(image, Feedback),
        log_file_name => maps:get(log_file_name, Feedback),
        log_file_data => maps:get(log_file_data, Feedback),
        is_read => maps:get(is_read, Feedback),
        remarks => maps:get(remarks, Feedback)
    }.

get_player_feedback(PlayerID)->
    Records = db:query("select * from player_feedback where player_id=" ++ PlayerID),%%db:get(player_feedback, PlayerID),
    [begin
         Player = player:get_player(maps:get(player_id, Feedback)),
         MessageList = io_lib:format("~ts",[xmerl_ucs:from_utf8(binary_to_list(maps:get(content, Feedback)))]),
         Msg = unicode:characters_to_binary(MessageList),
         #{
             send_time => datetime:datetime_to_string(maps:get(send_time, Feedback)),
             player_id => maps:get(player_id, Feedback),
             name => Player #player.name,
             content => Msg,
             image_name => maps:get(image_name, Feedback),
             image => maps:get(image, Feedback),
             log_file_name => maps:get(log_file_name, Feedback),
             log_file_data => maps:get(log_file_data, Feedback),
             is_read => maps:get(is_read, Feedback),
             remarks => maps:get(remarks, Feedback)
         }
     end || Feedback <- Records].

get_file_data(FeedbackId)->
    case db:get(player_feedback, FeedbackId) of
        [] -> [];
        [R]->
          maps:get(log_file_name, R)
    end.

get_all_data()->
    Records = db:query("select * from player_feedback order by send_time desc"),
    [begin
         FeedbackId = maps:get(feedback_id, Feedback),
         ImageData  = maps:get(image, Feedback),
         Logfile    = maps:get(log_file_data, Feedback),
         make_file(io_lib:format("~p.jpg", [FeedbackId]), ImageData),
         make_file(io_lib:format("~p.txt", [FeedbackId]), Logfile)
     end || Feedback <- Records].

make_file(Title, ImageData)->
    lager:info("make file ~p", [Title]),
    {ok, S} = file:open(Title, write),
    file:write(S, ImageData),
    file:close(S).

get_file_path() ->
    case application:get_env(game, feedback_file_path) of
        undefined -> "../../tools/dgry_admin/priv/static/";
        {ok, Path} -> Path
    end.

set_read(FeedbackId)->
    case db:get(player_feedback, FeedbackId) of
        [] -> [];
        [R]->
            db:save(player_feedback, R #{is_read=>1})
    end.

set_remark(FeedbackId, Remarks)->
    case db:get(player_feedback, FeedbackId) of
        [] -> [];
        [R]->
            lager:debug("save remarks ~p ~p", [FeedbackId, Remarks]),
            db:save(player_feedback, R #{remarks=>Remarks})
    end.
-ifndef(_knowledge_types_included).
-define(_knowledge_types_included, yeah).

-define(knowledge_login_exception_errcode_UNKNOW_ERROR, 0).
-define(knowledge_login_exception_errcode_WRONG_PROTOCAL, 1).
-define(knowledge_login_exception_errcode_WRONG_PASSWORD, 2).
-define(knowledge_login_exception_errcode_UNKNOW_ACCOUNT, 3).
-define(knowledge_login_exception_errcode_GAME_SERVER_FULL, 4).
-define(knowledge_login_exception_errcode_GAME_SERVER_OFFLINE, 5).
-define(knowledge_login_exception_errcode_UNDER_ATTACKING, 6).

-define(knowledge_device_platform_IOS, 1).
-define(knowledge_device_platform_Android, 2).
-define(knowledge_device_platform_PC, 3).
-define(knowledge_device_platform_Other, 4).

-define(knowledge_set_player_name_errcode_PLAYER_NOT_FOUND, 1).
-define(knowledge_set_player_name_errcode_ILLIGAL_NAME_LENGTH, 2).

-define(knowledge_topic_HISTORY, 1).
-define(knowledge_topic_LIFE, 2).

-define(knowledge_rslt_type_WRONG, 0).
-define(knowledge_rslt_type_RIGHT, 1).

-define(knowledge_game_logic_message_type_SEND_QUESTION, 1).
-define(knowledge_game_logic_message_type_ANSWER_QUESTION, 2).

-define(knowledge_decuct_type_GAMEING_QUIT, 1).

-define(knowledge_fortune_type_BEAN, 1).

-define(knowledge_challenge_role_INVITER, 1).
-define(knowledge_challenge_role_INVITED, 2).

-define(knowledge_pk_type_PVP, 1).
-define(knowledge_pk_type_PVE, 2).

-define(knowledge_match_rslt_type_SUCCESS, 0).
-define(knowledge_match_rslt_type_FAIL, 1).

-define(knowledge_pk_errcode_NO_ROOM, 1).
-define(knowledge_pk_errcode_GAME_NO_START, 2).
-define(knowledge_pk_errcode_GAME_END, 3).
-define(knowledge_pk_errcode_HAS_MATCHED, 4).

-define(knowledge_game_error_NO_END, 1).

-define(knowledge_match_error_NO_ENOUGH_BEAN, 1).

-define(knowledge_game_status_GAME_STATUS_READY, 0).
-define(knowledge_game_status_GAME_STATUS_START, 1).
-define(knowledge_game_status_GAME_STATUS_END, 2).

-define(knowledge_pk_rslt_LOSE, 0).
-define(knowledge_pk_rslt_WIN, 1).
-define(knowledge_pk_rslt_DRAW, 2).

-define(knowledge_topic_tag_NORMAL, 0).
-define(knowledge_topic_tag_NEW, 1).
-define(knowledge_topic_tag_HOT, 2).

%% struct login_exception

-record(login_exception, {error_code :: integer(),
                          msg :: string() | binary()}).

%% struct get_player_exception

-record(get_player_exception, {msg :: string() | binary()}).

%% struct set_player_name_exception

-record(set_player_name_exception, {error_code :: integer(),
                                    msg :: string() | binary()}).

%% struct pk_exception

-record(pk_exception, {error_code :: integer()}).

%% struct game_exception

-record(game_exception, {error_code :: integer()}).

%% struct match_exception

-record(match_exception, {error_code :: integer()}).

%% struct datetime

-record(datetime, {year :: integer(),
                   month :: integer(),
                   day :: integer(),
                   hour :: integer(),
                   minute :: integer(),
                   second :: integer()}).

%% struct date

-record(date, {year :: integer(),
               month :: integer(),
               day :: integer()}).

%% struct time

-record(time, {hour :: integer(),
               minute :: integer(),
               second :: integer()}).

%% struct another_device_logined

-record(another_device_logined, {ip_address :: string() | binary(),
                                 port :: integer()}).

%% struct stop_server_msg

-record(stop_server_msg, {rest_time :: integer()}).

%% struct player

-record(player, {player_id :: integer(),
                 name :: string() | binary(),
                 higher_department :: string() | binary(),
                 department :: string() | binary(),
                 level :: integer(),
                 bean :: integer(),
                 experience :: integer(),
                 area :: string() | binary(),
                 wins :: integer(),
                 all_games :: integer(),
                 user_id :: integer(),
                 nickname :: string() | binary(),
                 head_url :: string() | binary(),
                 phone :: string() | binary(),
                 dept_id :: integer(),
                 post_id :: integer(),
                 post_name :: string() | binary(),
                 position_id :: integer(),
                 position_name :: string() | binary(),
                 right_answers :: integer(),
                 all_answers :: integer(),
                 all_answer_time :: integer()}).

%% struct player_info

-record(player_info, {player_id :: integer(),
                      name :: string() | binary(),
                      head_icon :: string() | binary(),
                      level :: integer(),
                      experience :: integer()}).

%% struct notify_match_info

-record(notify_match_info, {player_id :: integer(),
                            topic_id :: integer()}).

%% struct player_pk_info

-record(player_pk_info, {player_id :: integer(),
                         win_rate :: integer(),
                         name :: string() | binary(),
                         department :: string() | binary(),
                         level :: integer(),
                         head_url :: string() | binary(),
                         is_robot :: integer()}).

%% struct answer_rslt

-record(answer_rslt, {round_id :: integer(),
                      answer_id :: integer(),
                      answer_time :: integer(),
                      score :: integer()}).

%% struct top_topic_ranking_info

-record(top_topic_ranking_info, {player_id :: integer(),
                                 name :: string() | binary(),
                                 photo_url :: string() | binary()}).

%% struct topic_ranking_detail

-record(topic_ranking_detail, {top_list :: list(),
                               my_rank :: integer()}).

%% struct player_pk_rslt

-record(player_pk_rslt, {player_id :: integer(),
                         win_rate :: integer(),
                         name :: string() | binary(),
                         department :: string() | binary(),
                         level :: integer(),
                         head_url :: string() | binary(),
                         topic_exp :: integer(),
                         award_bean :: integer(),
                         award_exp :: integer(),
                         topic_bean :: integer(),
                         is_win :: integer(),
                         is_robot :: integer(),
                         answer_list :: list(),
                         topic_ranking_list :: list()}).

%% struct notify_start_pk

-record(notify_start_pk, {room_id :: integer(),
                          pk_info_list :: list(),
                          type :: integer()}).

%% struct notify_finish_pk

-record(notify_finish_pk, {winner_id :: integer()}).

%% struct notify_exit_pk

-record(notify_exit_pk, {exit_playerid :: integer()}).

%% struct notify_answer_question

-record(notify_answer_question, {player_id :: integer(),
                                 answer_id :: integer(),
                                 answer_time :: integer(),
                                 round_id :: integer()}).

%% struct notify_match_rslt

-record(notify_match_rslt, {type :: integer()}).

%% struct option_info

-record(option_info, {option_id :: integer(),
                      option_content :: string() | binary()}).

%% struct question_info

-record(question_info, {round :: integer(),
                        question_id :: integer(),
                        question :: string() | binary(),
                        options :: list(),
                        answer_id :: integer()}).

%% struct notify_question_list

-record(notify_question_list, {questions :: list()}).

%% struct notify_start_round

-record(notify_start_round, {curround :: integer()}).

%% struct notify_game_end

-record(notify_game_end, {room_id :: integer(),
                          topic_id :: integer(),
                          pk_rslt_list :: list()}).

%% struct notify_decuct_fortune

-record(notify_decuct_fortune, {d_type :: integer(),
                                f_type :: integer(),
                                value :: integer()}).

%% struct notify_match_ai

-record(notify_match_ai, {ai_id :: integer(),
                          ai_name :: string() | binary()}).

%% struct notify_player_reconnect

-record(notify_player_reconnect, {room_id :: integer(),
                                  player_id :: integer()}).

%% struct game_info

-record(game_info, {topic_id :: integer(),
                    room_id :: integer(),
                    questions :: list(),
                    curround :: integer(),
                    passtime :: integer(),
                    myscore :: integer(),
                    otherscore :: integer(),
                    pk_info_list :: list(),
                    my_answer_list :: list(),
                    other_answer_list :: list()}).

%% struct topic_detail

-record(topic_detail, {player_id :: integer(),
                       topic_id :: integer(),
                       wins :: integer(),
                       alls :: integer(),
                       right_answers :: integer(),
                       all_answers :: integer(),
                       all_answer_time :: integer(),
                       experiment :: integer(),
                       beans :: integer(),
                       last_challenge_time :: #datetime{}}).

%% struct challenge_topic_list

-record(challenge_topic_list, {topic_detail_list :: list()}).

%% struct player_detail

-record(player_detail, {player_id :: integer(),
                        room_id :: integer()}).

%% struct ai_info

-record(ai_info, {ai_id :: integer(),
                  ai_name :: string() | binary(),
                  department :: string() | binary(),
                  level :: integer(),
                  topic_exp :: integer(),
                  topic_bean :: integer(),
                  topic_id :: integer(),
                  room_id :: integer(),
                  curround :: integer(),
                  answer_time :: integer(),
                  answer_list :: list()}).

%% struct topic_info

-record(topic_info, {topic_id :: integer(),
                     topic_name :: string() | binary(),
                     tag :: integer()}).

%% struct personal_page_info

-record(personal_page_info, {player_info :: #player{},
                             top_player_topic :: list()}).

-endif.

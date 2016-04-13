-ifndef(_game_types_included).
-define(_game_types_included, yeah).

-define(game_device_platform_IOS, 1).
-define(game_device_platform_Android, 2).
-define(game_device_platform_PC, 3).
-define(game_device_platform_Other, 4).

-define(game_login_exception_errcode_UNKNOW_ERROR, 101001).
-define(game_login_exception_errcode_WRONG_PROTOCAL, 101002).
-define(game_login_exception_errcode_WRONG_PASSWORD, 101003).
-define(game_login_exception_errcode_UNKNOW_ACCOUNT, 101004).
-define(game_login_exception_errcode_GAME_SERVER_FULL, 101005).
-define(game_login_exception_errcode_GAME_SERVER_OFFLINE, 101006).
-define(game_login_exception_errcode_UNDER_ATTACKING, 101007).

-define(game_set_player_name_errcode_PLAYER_NOT_FOUND, 102001).
-define(game_set_player_name_errcode_ILLIGAL_NAME_LENGTH, 102002).

-define(game_feedback_errcode_SEND_QUICKLY, 103001).
-define(game_feedback_errcode_MESSAGE_ILLEGAL_CHARACTER, 103002).

%% struct login_exception

-record(login_exception, {error_code :: integer(),
                          msg :: string() | binary()}).

%% struct get_player_exception

-record(get_player_exception, {msg :: string() | binary()}).

%% struct set_player_name_exception

-record(set_player_name_exception, {error_code :: integer(),
                                    msg :: string() | binary()}).

%% struct feedback_exception

-record(feedback_exception, {error_code :: integer(),
                             msg :: string() | binary()}).

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
                 area :: string() | binary()}).

-endif.

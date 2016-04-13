%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2014 14:35
%%%-------------------------------------------------------------------
-module(record_file).
-author("long").

-define(ROOT_BASE, "record_data_").

%% API
-export([create_dir/0, set_record_dir/1, get_network_content/1, get_network_files/0, get_lastest_record_dir/0]).
-export([open_network_record/1, get_cache_record_name/0, get_mysql_record_name/0, get_datetime_record_name/0]).

create_dir() ->
    Datetime = datetime_real:localtime(),
    Str = datetime_tostring(Datetime),
    RecordDir = ?ROOT_BASE ++ Str,
    set_record_dir(RecordDir),
    NetworkDir = get_network_dir(),
    ok = filelib:ensure_dir(NetworkDir++"/").

set_record_dir(Dir) ->
    application:set_env(game, record_dir, Dir).

open_network_record(Pid) ->
    Filename = get_pid_string(Pid) ++ ".rcd",
    NetworkDir = get_network_dir(),
    Filename1 = filename:join([NetworkDir , Filename]),
    {ok, F} = file:open(Filename1, [write]),
    F.

get_cache_record_name() ->
    filename:join([get_record_dir(), "cache_records.rcd"]).

get_mysql_record_name() ->
    filename:join([get_record_dir(), "db_records.rcd"]).

get_datetime_record_name() ->
    filename:join([get_record_dir(), "time_records.rcd"]).

get_record_dir() ->
    {ok, RecordDir} = application:get_env(game, record_dir),
    RecordDir.

get_network_dir() ->
    RecordDir = get_record_dir(),
    filename:join([RecordDir, "network"]).

get_network_content(Filename) ->
    Path = lists:concat([get_network_dir(), "/", Filename]),
    {ok, Terms} = file:consult(Path),
    Terms.

get_network_files() ->
    {ok, Filenames} = file:list_dir(get_network_dir()),
    Filenames.

%% 获得最新的record_data目录
get_lastest_record_dir() ->
    {ok, Filenames} = file:list_dir("./"),
    F = fun(Filename, Acc) ->
            case string:str(Filename, ?ROOT_BASE) of
                0 -> Acc;
                _ -> [Filename | Acc]
            end
        end,
    L = lists:foldl(F, [], Filenames),
    get_lastest_record_dir(L).

get_lastest_record_dir(Filenames) ->
    F = fun(FN1, FN2) ->
        ST1 = get_seconds(FN1),
        ST2 = get_seconds(FN2),
        ST1 =< ST2
    end,
    L = lists:sort(F, Filenames),
    lists:last(L).

get_seconds(Filename) ->
    Index = string:str(Filename, ?ROOT_BASE),
    DT = string:substr(Filename, Index+length(?ROOT_BASE)),
    [Year, Month, Day, Hour, Minute, Second] = [list_to_integer(D) || D <- string:tokens(DT, "_")],
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}).

get_pid_string(Pid) ->
    PL = pid_to_list(Pid),
    lists:sublist(PL, 2, length(PL)-2).

datetime_tostring(#{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second}) ->
    lists:concat([Year, "_", Month, "_", Day, "_", Hour, "_", Minute, "_", Second]).

%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2014 17:28
%%%-------------------------------------------------------------------
-module(datetime).
-author("long").

%% API
-export([start_link/0, date/0, time/0, localtime/0, localdaytime/0]).
-export([timestamp/0, diff_timestamp/2, diff_datetime/2, add_timestamp/2,
    add_datetime/2, millisecond/1, millisecond_to_timestamp/1, get_timestamp/1, get_microsecond_timestamp/0, timestamp_to_datetime/1]).
-export([make_date/3, make_time/3, make_datetime/6]).
-export([datetime_to_string/1, datetime_from_string/1, translate/1]).
-export([is_between/3, is_lessthan/2, is_lessthan_equal/2, equal/2, max/2, is_today_pass_weekday/1, get_weekday_localtime/1, get_next_weekday_localtime/1]).

start_link() ->
    Module = get_module(),
    Module:start_link().

%% 返回当前日期, 返回格式:{date, {Year, Month, Day}}
date() ->
    Module = get_module(),
    Module:date().

%% 返回当前时间, 返回格式:{time, {Hour, Minute, Second}}
time() ->
    Module = get_module(),
    Module:time().

%% 返回当前日期和时间, 返回格式:{datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}
localtime() ->
    Module = get_module(),
    Module:localtime().

localdaytime()->
    Module = get_module(),
    #{year:=Year, month:=Month, day:=Day} = Module:date(),
    make_datetime(Year, Month, Day, 0, 0, 0).

%% 获得当前时间距离1970的TimeStamp值, 返回值格式参照os:timestamp()中的描述
timestamp() ->
    Module = get_module(),
    Module:timestamp().

%% 计算两个TimeStamp的毫秒数差
diff_timestamp(TimeStamp1, TimeStamp2) ->
    M1 = millisecond(TimeStamp1),
    M2 = millisecond(TimeStamp2),
    erlang:abs(M1 - M2).

%% 计算两个时间相差的秒数
diff_datetime(#{year:=Y1,month:=Mo1,day:=D1,hour:=H1,minute:=Mi1,second:=S1},
    #{year:=Y2,month:=Mo2,day:=D2,hour:=H2,minute:=Mi2,second:=S2}) ->
    Ss1 = calendar:datetime_to_gregorian_seconds({{Y1,Mo1,D1},{H1,Mi1,S1}}),
    Ss2 = calendar:datetime_to_gregorian_seconds({{Y2,Mo2,D2},{H2,Mi2,S2}}),
    Ss1 - Ss2.

is_between(Time, Time1, Time2) ->
    Diff1 = diff_datetime(Time, Time1),
    Diff2 = diff_datetime(Time, Time2),
    Diff1 >= 0 andalso Diff2 =< 0.

is_lessthan(Time1, Time2) ->
    Diff = diff_datetime(Time1, Time2),
    Diff < 0.

is_lessthan_equal(Time1, Time2) ->
    Diff = diff_datetime(Time1, Time2),
    Diff =< 0.

equal(Time1, Time2) ->
    Diff = diff_datetime(Time1, Time2),
    Diff == 0.

max(Time1, Time2) ->
    case is_lessthan(Time1, Time2) of
        true -> Time2;
        false -> Time1
    end.

%% 转换成毫秒数
millisecond({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

%% 把毫秒数转换成TimeStamp格式
millisecond_to_timestamp(MilliSecs) ->
    MegaSecs = MilliSecs div 1000000000,
    Millisecond = MilliSecs rem 1000,
    Secs = (MilliSecs - MegaSecs * 1000000000 - Millisecond) div 1000,
    {MegaSecs, Secs, Millisecond * 1000}.

%% 在TimeStamp的基础上, 增加AddMillisecond毫秒数
add_timestamp(TimeStamp, AddMillisecond) ->
    MS = millisecond(TimeStamp),
    MS1 = MS + AddMillisecond,
    millisecond_to_timestamp(MS1).

%% 在指定的日期的基础上增加Seconds秒数
add_datetime(#{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second}, Seconds) ->
    Sec = calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hour,Minute,Second}}),
    Sec1 = Sec + Seconds,
    translate(calendar:gregorian_seconds_to_datetime(Sec1)).

get_module() ->
    case application:get_env(game, datetime) of
        {ok, Module} -> Module;
        _ -> datetime_real
    end.

make_date(Year, Month, Day) ->
    #{year=>Year, month=>Month, day=>Day}.

make_time(Hour, Minute, Second) ->
    #{hour=>Hour, minute=>Minute, second=>Second}.

make_datetime(Year, Month, Day, Hour, Minute, Second) ->
    #{year=>Year, month=>Month, day=>Day, hour=>Hour, minute=>Minute, second=>Second}.


datetime_to_string(#{year:=Year, month:=Month, day:=Day,
    hour:=Hour, minute:=Minute, second:=Second}) ->
    lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]).

datetime_from_string(Time) when length(Time) == 19 ->
    Year = lists:sublist(Time, 1, 4),
    Month = lists:sublist(Time, 6, 2),
    Day = lists:sublist(Time, 9, 2),
    Hour = lists:sublist(Time, 12, 2),
    Minitue = lists:sublist(Time, 15, 2),
    Second = lists:sublist(Time, 18, 2),
    datetime:make_datetime(list_to_integer(Year),
        list_to_integer(Month),
        list_to_integer(Day),
        list_to_integer(Hour),
        list_to_integer(Minitue),
        list_to_integer(Second)
    ).

translate({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    make_datetime(Year, Month, Day, Hour, Minute, Second);
translate(#{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}};
translate(#{year:=Year, month:=Month, day:=Day}) ->
    {Year, Month, Day};
translate(#{hour:=Hour, minute:=Minute, second:=Second}) ->
    {Hour, Minute, Second}.


get_timestamp(#{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second})->
    get_timestamp({{Year, Month, Day}, {Hour, Minute, Second}});

get_timestamp(#{year:=Year, month:=Month, day:=Day})->
    get_timestamp({{Year, Month, Day}, {0, 0, 0}});

get_timestamp({{Y1,Mo1,D1},{H1,Mi1,S1}})->
    calendar:datetime_to_gregorian_seconds({{Y1,Mo1,D1},{H1,Mi1,S1}})-calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

get_microsecond_timestamp()->
    {Mes, S, MicroS} = erlang:timestamp(),
    Mes * 1000000000 + 1000 * S + MicroS div 1000.

is_today_pass_weekday(WeekDay)->
    DayNum  = calendar:day_of_the_week(erlang:date()),
    DayNum > WeekDay.

get_weekday_localtime(WeekDay) ->
    NowSecond = get_timestamp(localtime()),
    {Date, Time} = erlang:localtime(),

    DayNum  = calendar:day_of_the_week(Date),

    DayLeft = DayNum - WeekDay,

    SecondFromToday = calendar:time_to_seconds(Time),

    (NowSecond - SecondFromToday) - 86400 * DayLeft.

get_next_weekday_localtime(WeekDay) ->
    get_weekday_localtime(WeekDay) + 86400 * 7.
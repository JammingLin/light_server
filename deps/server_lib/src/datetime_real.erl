%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2014 17:28
%%%-------------------------------------------------------------------
-module(datetime_real).
-author("long").

%% API
-export([start_link/0, date/0, time/0, localtime/0, timestamp/0]).

start_link() ->
    {ok, self()}.

date() ->
    TS = os:timestamp(),
    {{Year, Month, Day}, _} = calendar:now_to_universal_time(TS),
    datetime:make_date(Year, Month, Day).

time() ->
    TS = os:timestamp(),
    {_, {Hour, Minute, Second}}  = calendar:now_to_universal_time(TS),
    datetime:make_time(Hour, Minute, Second).

localtime() ->
    %% 采用os:timestamp()的函数, 直接调用操作系统的时间, 速度很快,
    %% 如果调用 erlang:localtime(), 因为这个函数需要考虑时间同步等因素, 速度较慢
    %% 具体可以看 http://blog.yufeng.info/archives/2977#more-2977
    TS = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(TS),
    datetime:make_datetime(Year, Month, Day, Hour, Minute, Second).

timestamp() ->
    os:timestamp().

%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 八月 2014 14:54
%%%-------------------------------------------------------------------
-module(test_notification).
-author("Administrator").

%% API
-export([start/0]).

start() ->
    {ok, C} = eredis_sub:start_link(),
    eredis_sub:controlling_process(C),
    eredis_sub:psubscribe(C, [<<"*">>]),
    loop(C).

loop(Sub) ->
    receive
        M ->
            io:format("M:~p~n", [M]),
            eredis_sub:ack_message(Sub),
            loop(Sub)
    end.

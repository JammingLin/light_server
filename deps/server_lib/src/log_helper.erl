%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 三月 2015 上午11:33
%%%-------------------------------------------------------------------
-module(log_helper).
-author("linzhiwei").

-include("log_email.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% Supervisor callbacks

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    start_log_helper/1
]).


-define(LOG_HELPER_CRON, log_helper_cron).
-define(LOG_HELPER_STATUS, log_helper_status).
-define(ON, on).
-define(OFF, off).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-record(state, {config}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(_ProcName,Config) ->
    gen_server:start_link(?MODULE, [Config], [{timeout, 5 * 3600 * 1000}]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Config]) ->
    %% 转换配置
    Cfg = log_email:init_config(Config),

    %% 开启发送模式
    if
        Cfg #email_config.send_flag =:= true -> start_log_helper(Cfg);
        true ->
            ok
    end,

    {ok, #state{config = Cfg}}.

start_log_helper (Config) ->
    {_, NowTime} = erlang:localtime(),
    InTime = [S || S <- Config #email_config.send_time, S =:= NowTime],
    %% 如果到点发送
    if
        length(InTime) > 0 ->
             start_to_send(Config);
        true -> ok
    end,
    %% 设置发送定时器
    set_timer(Config).

set_timer(Config)->
    {Date, NowTime} = erlang:localtime(),
    TimeStageList   = Config #email_config.send_time,
    TodayList   = [TodaySend || TodaySend <- TimeStageList, TodaySend > NowTime],
    NextDayList = [NextdaySend || NextdaySend <- TimeStageList, NextdaySend < NowTime],
    NextTime = if
                   TodayList =/= [] -> log_email:get_timestamp({Date,hd(TodayList)}); %% 发送今天的
                   true ->
                       log_email:get_timestamp({Date,hd(NextDayList)}) + 86400 * Config #email_config.timeinterval
               end,

    Time = max(NextTime - log_email:get_local_timestamp(), 1),

    if
        Config #email_config.is_debug > 0  ->
            erlang:send_after(
                Config #email_config.is_debug * 1000,
                self(),
                send_email
            );
        true ->
            erlang:send_after(
                Time * 1000,
                self(),
                send_email
            )
    end.

start_to_send(Config) ->
    log_email:start_to_send_email(Config).

%%%%%%%%%%%%%%%%gen_server API%%%%%%%%%%%%%%%%%%%%%%
handle_info(send_email, State)->
    start_to_send(State #state.config),
    set_timer(State #state.config),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(game_player).
-include("game_types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4, stop/1]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([remote_call/3]).

-compile(export_all).

-define(SERVER, ?MODULE).

-record(state, {data_loaded = false, player_id, account, area}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(NetPid, PlayerID, Account, Area) ->
	gen_server:start_link(?MODULE, [NetPid, PlayerID, Account, Area], []).

stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

remote_call(Pid, Function, Params) when is_pid(Pid) ->
    gen_server:call(Pid, {call_func, Function, Params}, 1000 * 60).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([NetPid, PlayerID, Account, Area]) ->
    %% 注意:此函數不可以做阻塞的邏輯
    put(net_pid, NetPid),

    {ok, #state{player_id = PlayerID, account = Account, area = Area}}.

handle_call({call_func, Function, Params}, _From, #state{data_loaded = DataLoaded} = State) ->
    lager:debug("Processed ~p(~p)~n", [Function, Params]),
    Timeout = 5 * 1000,
    try
		case DataLoaded of
			false ->
                PlayerID = State#state.player_id,
                Account  = State#state.account,
                Area     = State#state.area,
                player:init(PlayerID, Account, Area, <<"">>);
			true -> ok
		end,
        Result = apply(?MODULE, Function, [Params]),
        lager:debug("Function(~p) Result ~p~n", [Function, Result]),
        {reply, Result, State#state{data_loaded=true}, Timeout}
    catch
        throw:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            lager:debug("Function(~p) Catch throw, ~p, ~p, ~p", [Function, State#state.player_id, Reason, Stacktrace]),
            {reply, {error, throw, Reason, Stacktrace}, State, Timeout}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
%%     lager:error("player rpc timeout"),
    {noreply, State, hibernate};
handle_info(_Info, State) ->
%% 	error_logger:error_msg("player rpc info :~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------handle_function ----------------------------------

%% 获取玩家数据
get_player({PlayerId}) ->
    Data = player:get_player(PlayerId),
    lager:info("get player ~p ~p", [PlayerId, Data]),
    {reply, Data}.

%% 获取服务端时间（UTC）
get_server_time(_Params) ->
    DateTime = datetime:localtime(),
    Data = translate:to_record(datetime, DateTime),
    {reply, Data}.

feedback({Message, Title, Image, Logfile}) ->
    PlayerID = get(player_id),
    feedback:handle_feedback(PlayerID, Message, Title, Image, Logfile),
    ok.

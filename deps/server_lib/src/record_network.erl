%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 八月 2014 11:52
%%%-------------------------------------------------------------------
-module(record_network).
-author("long").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([record/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {files,
    start_timestamp}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

record(Value) ->
    case application:get_env(game, record_data) of
        {ok, true} ->
            Parent = self(),
            TimeStamp = datetime_real:timestamp(),
            gen_server:cast(?SERVER, {Parent, TimeStamp, Value});
        _ -> ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{files=dict:new(),
        start_timestamp = datetime_real:timestamp()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({Pid, TimeStamp, Value}, #state{files=Files,
        start_timestamp = StartTimeStamp}=State) ->

    {F1, Files1} =
        case dict:find(Pid, Files) of
            error ->
                F = record_file:open_network_record(Pid),
                io:format(F, "~w.~n", [{start_timestamp, StartTimeStamp}]),
                {F, dict:append(Pid, F, Files)};
            {ok, [F]} -> {F, Files}
        end,
    DiffTime = datetime:diff_timestamp(TimeStamp, StartTimeStamp),
    io:format(F1, "{~w, ~w}.~n", [DiffTime, Value]),
    case Value of
        {error, closed} -> file:close(F1); %% 网络关闭, 需要释放文件句柄
        _ -> ok
    end,
    {noreply, State#state{files=Files1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
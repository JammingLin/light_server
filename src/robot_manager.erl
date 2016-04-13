%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 六月 2015 下午3:51
%%%-------------------------------------------------------------------
-module(robot_manager).
-author("linzhiwei").
-include("game_types.hrl").

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {robot_list, all_robot_pids, robot_start, robot_end, spawn_span}).

-define(SERVER, ?MODULE).

-compile(export_all).

run(RobotList, Start, End, SpawnSpan) ->
    lager:start(),
    lager:set_loglevel(lager_console_backend,debug),
    m_rand:start_link(),
    ?MODULE:start_link(RobotList, Start, End, SpawnSpan),
    timer:sleep(infinity).

start_link(RobotList, Start, End, SpawnSpan) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [RobotList, Start, End, SpawnSpan], []).

start_robot(RobotList, Start, End, SpawnSpan) ->
    ListLen    = length(RobotList),
    ALL        = End-Start+1,
    Average    = round(ALL/ListLen),
    ChildSpawn = SpawnSpan,
    {AllPid, _} = lists:foldl(fun({RobotNode,ServerIP, Port, DebugType}, {PidList,N}) ->
        ChildStart = if
                         N =:= 0-> Start;
                         true -> Start + N * Average
                     end,
        ChildEnd   = if
                         (N+1) =:= ListLen -> End;
                         true -> Start + (N + 1) * Average-1
                     end,
        lager:debug("~p start robot ~p ~p ~p", [RobotNode, ChildStart, ChildEnd, ChildSpawn]),
        R = rpc:call(RobotNode, robot, start_robot, [ServerIP, true, ChildStart, ChildEnd, ChildSpawn, Port, DebugType]),
        lager:debug("rpc call robot node result : ~p", [R]),
        timer:sleep(ChildSpawn),
        NewL = if
                   is_list(R) =:= true -> lists:append(PidList, R);
                   true -> PidList
               end,
        {NewL, N+1}
    end,
        {[],0},
        RobotList),
    AllPid.

restart(ClearTime)->
    gen_server:cast(?MODULE, {restart, ClearTime}).

close()->
    gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([RobotList, Start, End, SpawnSpan]) ->
    process_flag(trap_exit, true),
    AllPid = start_robot(RobotList, Start, End, SpawnSpan),
    {ok, #state{
        robot_list     = RobotList,
        all_robot_pids = AllPid,
        robot_start    = Start,
        robot_end      = End,
        spawn_span     = SpawnSpan
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({restart, ClearTime}, State)->
    [begin exit(Pid, shutdown) end || Pid <- State #state.all_robot_pids],
    timer:sleep(ClearTime*1000),
    NewPids = start_robot(State #state.robot_list,
        State #state.robot_start,
        State #state.robot_end,
        State #state.spawn_span),
    {noreply, State #state{ all_robot_pids = NewPids}};

handle_cast(stop, State)->
    lager:debug("handle cast stop"),
    [begin exit(Pid, shutdown) end || Pid <- State #state.all_robot_pids],
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, _TimerRef, {Pid, TimerName}}, State) ->
    Pid!{timeout, TimerName},
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    lager:debug("robot_manager terminate"),
    [begin exit(Pid, shutdown) end || Pid <- State #state.all_robot_pids],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

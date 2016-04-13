%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2015 下午5:21
%%%-------------------------------------------------------------------
-module(db_timer_save).
-author("jie").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, can_save/0, async_save_to_db/1, sync_save_to_db/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_DB_TRY_TIMES, 30).

-record(state, {}).

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

stop() ->
    gen_server:call(?MODULE, stop, 60 * 60 * 1000).

can_save() ->
    try
        ok == gen_server:call(?SERVER, can_save, 100) andalso
            true =/= get(sync_save_to_db_error)
    catch
        exit:{timeout, _} ->
            false
    end.

async_save_to_db(Tid) ->
    gen_server:cast(?SERVER, {save, Tid}).

sync_save_to_db(Tid) ->
    DbCommands = translate_queue_to_dbcommands(Tid),
    try
        execute_dbcommands(DbCommands),
        ets:delete_all_objects(Tid),
        ok
    catch
        Class:Reason ->
            put(sync_save_to_db_error, true),
            Stacktrace = erlang:get_stacktrace(),
            lager:error("sync_save_to_db, Class:~p,Reason:~p,StackTrace:~p", [Class, Reason, Stacktrace]),
            %% 进程出错是否需要做特殊处理, 是否通知主进程关闭服务器
            db_timer:stop_when_db_error(),
            error
    end.

%% 把数据根据Index放入到List中, 从而保证数据库的写入次序是符合预期的
translate_queue_to_dbcommands(EtsTid) ->
    L = ets:foldl(
        fun({Key, L}, Acc) ->
            lists:foldl(
                fun(E, AccL) ->
                    V = case E of
                            {save, Index, Record} ->
                                {Index, Key, Record, save};
                            {save, Index, PrimaryValue2, Record} ->
                                {Index, Key, PrimaryValue2, Record, save};
                            {delete, Index} ->
                                {Index, Key, delete};
                            {delete, Index, PrimaryValue2} ->
                                {Index, Key, PrimaryValue2, delete}
                        end,
                    [V | AccL]
                end, Acc, L)
        end, [], EtsTid),
    lists:keysort(1, L).

execute_dbcommands(DbCommands) ->
    %% 对DB错误,进行简单的容错处理(避免一些偶发性超时)
    lists:foreach(fun(Cmd) ->
        execute_dbcommand(Cmd, ?MAX_DB_TRY_TIMES)
    end, DbCommands),
    ok.

execute_dbcommand(Cmd) ->
    case Cmd of
        {_Index, {TableName, _PrimaryValue}, Record, save} ->
            db_real:save(TableName, Record);
        {_Index, {TableName, _PrimaryValue1}, _PrimaryValue2, Record, save} ->
            db_real:save(TableName, Record);
        {_Index, {TableName, PrimaryValue}, delete} ->
            db_real:delete(TableName, PrimaryValue);
        {_Index, {TableName, PrimaryValue1}, PrimaryValue2, delete} ->
            db_real:delete(TableName, PrimaryValue1, PrimaryValue2)
    end.

execute_dbcommand(Cmd, RestTryTimes) when RestTryTimes < 1 ->
    execute_dbcommand(Cmd);
execute_dbcommand(Cmd, RestTryTimes) when is_integer(RestTryTimes) ->
    try
        execute_dbcommand(Cmd)
    catch
        exit:{timeout, _} ->
            %% exit:{timeout,{gen_server,call,[mysql_dispatcher,{fetch,mysql,_SQL},1]}} ->
            TryTimes = ?MAX_DB_TRY_TIMES - RestTryTimes + 1,
            Sleep = TryTimes * 1000,
            lager:info("execute_dbcommand timeout times:~p", [TryTimes]),
            timer:sleep(Sleep),
            execute_dbcommand(Cmd, RestTryTimes - 1);
        Class:Reason ->
            Stack = erlang:get_stacktrace(),
            erlang:raise(Class, Reason, Stack)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init([]) ->
    {ok, #state{}}.

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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(can_save, _From, State) ->
    {reply, ok, State};
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
handle_cast({save, Tid}, State) ->
    sync_save_to_db(Tid),
    {noreply, State};
handle_cast({is_save_to_db_error}, State) ->

    {noreply, State};
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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
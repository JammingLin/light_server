%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2015 下午1:29
%%%-------------------------------------------------------------------
-module(db_mysql_async).
-author("jie").

-behaviour(gen_server).

%% API
-export([start/1, start/2, stop/0, start_link/1]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([pool_execute/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(WRITE_POOL, write_pool).
-record(state, {}).

start(SqlPath) ->
    {ok, [Pool|_]} = application:get_env(db, db_pools),
    start([Pool], SqlPath).

start(Pools, SqlPath) ->
    new_pool(10, 20),
    db_mysql_common:start(Pools),
    db_update:start(Pools, SqlPath).

stop() ->
    poolboy:stop(?WRITE_POOL),
    db_mysql_common:stop().

query(Query) ->
    db_mysql_common:query(Query).

query(_Key, Query) ->
    db_mysql_common:query(Query).

execute(Sql) ->
    poolboy:transaction(?WRITE_POOL, fun(Worker) ->
        gen_server:call(Worker, {save, Sql})
    end).

execute(_Key, Sql) ->
    poolboy:transaction(?WRITE_POOL, fun(Worker) ->
        gen_server:call(Worker, {save, Sql})
    end).

execute2(Query) ->
    db_mysql_common:execute2(Query).

execute2(_Key, Query) ->
    db_mysql_common:execute2(Query).

pool_execute(PoolName, Sql) ->
    poolboy:transaction(?WRITE_POOL, fun(Worker) ->
        gen_server:call(Worker, {pool_save, PoolName, Sql})
    end).

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
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

new_pool(Size, MaxOverflow) ->
    poolboy:start_link([{name, {local, ?WRITE_POOL}},
        {worker_module, ?MODULE},
        {size, Size}, {max_overflow, MaxOverflow}], []).
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
handle_call({save, Sql}, _From, State) ->
    Result = db_mysql_common:execute2(Sql),
    case Result of
        {error, Ret} -> lager:error("save error, Ret:~p, Sql:~p", [Ret, Sql]);
        _ -> ok
    end,
    {reply, Result, State};
handle_call({pool_save, PoolName, Sql}, _From, State) ->
    Result = db_mysql_common:execute2(PoolName, Sql),
    case Result of
        {error, Ret} -> lager:error("pool_save error, Ret:~p, Sql:~p", [Ret, Sql]);
        _ -> ok
    end,
    {reply, Result, State};
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

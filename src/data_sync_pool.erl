%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2014 18:03
%%%-------------------------------------------------------------------
-module(data_sync_pool).
-author("Administrator").

-behaviour(gen_server).

%% API
-export([start_link/2, stop/0, get/1, del/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {workers = dict:new(), waitings = queue:new()}).
-define(POOL_NAME, data_sync).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Module, PoolSize) ->
    {ok, _} = hottub:start_link(?POOL_NAME, PoolSize, Module, start_link, []),
    %% TODO: 分布式系统的支持与效率问题, (分布式事务)
    %% TODO: 算法: 可以按pid来划分到多个进程进行处理, 避免单点
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    hottub:stop(?POOL_NAME).

get(Key) ->
    gen_server:call({global, ?SERVER}, {get, Key}).

del(Key, Pid) ->
    gen_server:cast({global, ?SERVER}, {del, Key, Pid}).

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
    {ok, #state{workers =dict:new(), waitings =queue:new()}}.

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
handle_call({get, Key}, {FromPid, _Tag}=From, #state{workers = Workers}=State) ->
    case dict:find(Key, Workers) of
        error ->
          case nonblock_checkout_worker(FromPid) of
              empty ->
                  {noreply, State#state{waitings = queue:in(From, State#state.waitings)}};
              Pid ->
                  {reply, Pid, State#state{workers=dict:append(Key, Pid, Workers)}}
          end;
        {ok, [Pid]} ->
            {reply, Pid, State}
    end;
handle_call(Request, From, S) ->
    %%error_logger:warning_msg("The data_sync_pool server received an unexpected message:\n"
    %%    "handle_call(~p, ~p, _)\n",[Request, From]),
    lager:warning("The data_sync_pool server received an unexpected message:\n"
        "handle_call(~p, ~p, _)\n",[Request, From]),

{noreply, S}.

nonblock_checkout_worker(From) ->
    Node = node(From),
    rpc:call(Node, ht_pool, nonblock_checkout_worker, [?POOL_NAME, infinity]).

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
handle_cast({del, Key, Worker}, #state{workers = Workers}=State) ->
    Workers1 = dict:erase(Key, Workers),
    case queue:out(State#state.waitings) of
        {{value, P}, Waitings} ->
            gen_server:reply(P, Worker),
            {noreply, State#state{workers = Workers1, waitings =Waitings}};
        {empty, _Waitings} ->
            ht_pool:checkin_worker(?POOL_NAME, Worker),
            {noreply, State#state{workers = Workers1}}
    end;
handle_cast(Request, State) ->
    %%error_logger:warning_msg("The data_sync_pool server received an unexpected message:\n"
    %%    "handle_cast(~p, _)\n",[Request]),
    lager:warning("The data_sync_pool server received an unexpected message:\n"
        "handle_cast(~p, _)\n",[Request]),
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

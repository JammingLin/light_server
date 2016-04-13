%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Feb 2015 下午2:07
%%%-------------------------------------------------------------------
-module(monitor_nodes).
-author("jie").

-behaviour(gen_server).

%% API
-export([start_link/0, register_gate/1, get_gate_node/0, get_db_node/0, get_db_node/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(RECONNECT_TIME, 2000).
-record(state, {gate_node}).

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

get_gate_node() ->
    gen_server:call(?SERVER, get_gate).

get_db_node() ->
    get_db_node([]).
get_db_node([]) ->
    case init:get_argument(db_server) of
        error -> error;
        {badrpc,_} -> error;
        {ok, [[Node]]} -> list_to_atom(Node)
    end;
get_db_node(DBNode) ->
    DBNode.

register_gate(GateNode) ->
    gen_server:call(?SERVER, {register_gate, GateNode}).

%% join_gate(GateNode) ->
%%     %% 和网关服的连通, 放到最后, 就是直到所有的准备初始化工作都做完, 才连接到网关服务器, 开始对玩家服务
%%     try
%%         gen_server:call({game_node_manager, GateNode}, {add_node, node()}),
%%         %% error_logger:info_msg("join gate ok, cookie:~p, self node:~p, gate node:~p~n",
%%         lager:info("join gate ok, cookie:~p, self node:~p, gate node:~p~n",
%%             [erlang:get_cookie(), node(), GateNode])
%%     catch exit:{noproc, _}=Reason ->
%%         timer:sleep(1000),
%%         lager:info("join gate error, reason:~p, rejoin", [Reason]),
%%         join_gate(GateNode)
%%     end.

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
%%     net_kernel:monitor_nodes(true),
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
handle_call({register_gate, GateNode}, _From, State) ->
    lager:info("register gate ~p success", [GateNode]),
    {reply, ok, State#state{gate_node = GateNode}};
handle_call(get_gate, _From, State) ->
    {reply, State#state.gate_node, State};
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
%% handle_info({nodedown, Node}, #state{}=State) ->
%%     lager:error("node down : ~p", [Node]),
%%     timer:apply_after(?RECONNECT_TIME, ?MODULE, reconnect, [Node]),
%%     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% reconnect(Node) ->
%%     case Node == get_gate_node() of
%%         true ->
%%             case net_adm:ping(Node) of
%%                 pong ->
%%                     join_gate(Node),
%%                     ok;
%%                 pang -> timer:apply_after(?RECONNECT_TIME, ?MODULE, reconnect, [Node])
%%             end;
%%         false -> ok
%%     end,
%%     case Node == get_db_node() of
%%         true ->
%%             case net_adm:ping(Node) of
%%                 pong -> ok;
%%                 pang -> timer:apply_after(?RECONNECT_TIME, ?MODULE, reconnect, [Node])
%%             end;
%%         false -> ok
%%     end.

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

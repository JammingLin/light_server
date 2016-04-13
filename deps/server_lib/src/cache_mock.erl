%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:24
%%%-------------------------------------------------------------------
-module(cache_mock).
-author("long").

-behaviour(gen_server).

-include("cache.hrl").

%% API
-export([start/1, start_link/0, start_link/1]).
-export([q/1, q/2]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {terms}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start(_Port) ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(_Port) ->
    start_link().

q(Command) ->
    q(Command, ?TIMEOUT).

q(Command, Timeout) ->
    gen_server:call(?SERVER, {Command, Timeout}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    Filename = record_file:get_cache_record_name(),
    {ok, Terms} = file:consult(Filename),
    {ok, #state{terms = Terms}}.

handle_call({Command, Timeout}, _From, #state{terms = Terms}=State) ->
    [{Command, Timeout, Result} | Terms1] = Terms,%% 如果不匹配, 那么可能是执行的顺序不一致导致的
    {reply, Result, State#state{terms = Terms1}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
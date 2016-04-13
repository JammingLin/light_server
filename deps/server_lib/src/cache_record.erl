%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:24
%%%-------------------------------------------------------------------
-module(cache_record).
-author("long").

-include("cache.hrl").

-behaviour(gen_server).

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

-record(state, {file}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start(Port) ->
	{ok, _} = cache_redis:start(Port),
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    {ok, _} = cache_redis:start_link(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(_Port) ->
    start_link().

q(Command) ->
    q(Command, ?TIMEOUT).

q(Command, Timeout) ->
    Result = cache_redis:q(Command, Timeout),
    gen_server:cast(?SERVER, {Command, Timeout, Result}),
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    FileName = record_file:get_cache_record_name(),
    {ok, F} = file:open(FileName, [write]),
    {ok, #state{file=F}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(Record, #state{file=F}=State) ->
    io:format(F, "~w.~n", [Record]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
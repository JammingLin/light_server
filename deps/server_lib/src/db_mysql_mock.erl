%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:24
%%%-------------------------------------------------------------------
-module(db_mysql_mock).
-author("long").

-behaviour(gen_server).

-include_lib("emysql.hrl").
%% API
-export([start/0]).
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
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
start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

query(Query) ->
    {TimeConsume, Result} = gen_server:call(?SERVER, {query, Query}),
    timer:sleep(TimeConsume),
    Result.
query(Query, Args) ->
    {TimeConsume, Result} = gen_server:call(?SERVER, {query, Query, Args}),
    timer:sleep(TimeConsume),

    Result.

execute(Query) ->
    Result = execute2(Query),
    case Result of
        #ok_packet{affected_rows = Rows} ->Rows;
        _ -> Result
    end.
execute(Query, Args) ->
    Result = execute2(Query, Args),
    case Result of
        #ok_packet{affected_rows = Rows} -> Rows;
        _ ->  Result
    end.

execute2(Query) ->
    {TimeConsume, Result} = gen_server:call(?SERVER, {execute, Query}),
    timer:sleep(TimeConsume),
    Result.
execute2(Query, Args) ->
    {TimeConsume, Result} = gen_server:call(?SERVER, {execute, Query, Args}),
    timer:sleep(TimeConsume),
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    Filename = record_file:get_mysql_record_name(),
    {ok, Terms} = file:consult(Filename),
    {ok, #state{terms = Terms}}.

handle_call({query, Query}, _From, #state{terms = Terms}=State) ->
    [{TimeConsume, Query, Result} | Terms1] = Terms,%% 如果不匹配, 那么可能是执行的顺序不一致导致的
    {reply, {TimeConsume, Result}, State#state{terms = Terms1}};
handle_call({query, Query, Args}, _From, #state{terms = Terms}=State) ->
    [{TimeConsume, Query, Args, Result} | Terms1] = Terms,%% 如果不匹配, 那么可能是执行的顺序不一致导致的, 或者参数不一致
    {reply, {TimeConsume, Result}, State#state{terms = Terms1}};
handle_call({execute, Query}, _From, #state{terms = Terms}=State) ->
    [{TimeConsume, Query, Result} | Terms1] = Terms,%% 如果不匹配, 那么可能是执行的顺序不一致导致的
    {reply, {TimeConsume, Result}, State#state{terms = Terms1}};
handle_call({execute, Query, Args}, _From, #state{terms = Terms}=State) ->
    [{TimeConsume, Query, Args, Result} | Terms1] = Terms,%% 如果不匹配, 那么可能是执行的顺序不一致导致的, 或者参数不一致
    {reply, {TimeConsume, Result}, State#state{terms = Terms1}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
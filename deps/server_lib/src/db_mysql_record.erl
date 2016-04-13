%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:57
%%%-------------------------------------------------------------------
-module(db_mysql_record).
-author("long").

-include_lib("emysql.hrl").

%% 对db_mysql模块的一些函数做参数和结果的记录
-export([start/0]).

%% API
-export([query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([record/3, record/4]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {file}).

start() ->
    db_mysql_common:start(),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

query(Query) ->
    {MicroSecs, Result} = timer:tc(emysql, execute, [mysql, Query]),
    Records = db_mysql_common:to_map(Result),
    record(MicroSecs div 1000, Query, Records),
    Records.

query(Query, Args) ->
    {MicroSecs, Result} = timer:tc(emysql, execute, [mysql, Query, Args]),
    Records = db_mysql_common:to_map(Result),
    record(MicroSecs div 1000, Query, Args, Records),
    Records.

execute(Query) ->
    Result = execute2(Query),
    case Result of
        #ok_packet{affected_rows = Rows} ->
            Rows;
        _ ->
            Result
    end.

execute(Query, Args) ->
    Result = execute2(Query, Args),
    case Result of
        #ok_packet{affected_rows = Rows} ->
            Rows;
        _ ->
            Result
    end.

execute2(Query) ->
    {MicroSecs, Result} = timer:tc(emysql, execute, [mysql, Query]),
    record(MicroSecs div 1000, Query, Result),
    Result.

execute2(Query, Args) ->
    {MicroSecs, Result} = timer:tc(emysql, execute, [mysql, Query, Args]),
    record(MicroSecs div 1000, Query, Args, Result),
    Result.

record(TimeConsume, Query, Result) ->
    gen_server:cast(?SERVER, {TimeConsume, Query, Result}).

record(TimeConsume, Query, Args, Result) ->
    gen_server:cast(?SERVER, {TimeConsume, Query, Args, Result}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    FileName = record_file:get_mysql_record_name(),
    {ok, F} = file:open(FileName, [write]),
    {ok, #state{file=F}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({TimeConsume, Query, Result}, #state{file=F}=State) ->
    io:format(F, "{~w, ~w, ~w}.~n", [TimeConsume, Query, Result]),
    {noreply, State};
handle_cast({TimeConsume, Query, Args, Result}, #state{file=F}=State) ->
    io:format(F, "{~w, ~w, ~w, ~w}.~n", [TimeConsume, Query, Args, Result]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
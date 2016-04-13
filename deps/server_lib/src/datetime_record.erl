%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2014 17:28
%%%-------------------------------------------------------------------
-module(datetime_record).
-author("long").

-define(SERVER, ?MODULE).
-record(state, {file}).

-export([start_link/0]).
-export([record/1]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% API
-export([date/0, time/0, localtime/0, timestamp/0]).

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

record(Val) ->
    gen_server:cast(?SERVER, Val).

date() ->
    Date = datetime_real:date(),
    record(Date),
    Date.

time() ->
    Time = datetime_real:time(),
    record(Time),
    Time.

localtime() ->
    DateTime = datetime_real:localtime(),
    record(DateTime),
    DateTime.

timestamp() ->
    TimeStamp = datetime_real:timestamp(),
    record({timestamp, TimeStamp}),
    TimeStamp.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    FileName = record_file:get_datetime_record_name(),
    {ok, F} = file:open(FileName, [write]),
    {ok, #state{file=F}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Val, #state{file=F}=State) ->
    io:format(F, "~w.~n", [Val]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

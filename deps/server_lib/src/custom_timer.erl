%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2015 10:55
%%%-------------------------------------------------------------------
-module(custom_timer).
-author("Administrator").
-define(SERVER, ?MODULE).

%% API
-export([mark/0]).
-export([start_link/1, init/1, handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-record(state, {start_time,
    end_time,
    count = 0,
    max_count}).

start_link(MaxCount) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxCount], []).

init([MaxCount]) ->
    {ok, #state{start_time = os:timestamp(), count = 0, max_count = MaxCount}}.

mark() ->
    gen_server:call(?SERVER, mark).

handle_call(mark, _From, State) ->
    NewState = case State of
                   #state{count = Count, max_count = MaxCount} ->
                       case Count + 1 == MaxCount of
                           true ->
                               State#state{end_time = os:timestamp()};
                           false ->
                               State#state{count = Count + 1}
                       end;
                   _ ->
                       error
               end,

    Result = case NewState of
                 #state{end_time = undefined} ->
                     ok;
                 #state{start_time = ST, end_time = ET} ->
                     {_S1, S2, S3} = ST,
                     {_E1, E2, E3} = ET,
                     ((E2 * 1000000 + E3) - (S2 * 1000000 + S3)) / 1000
             end,
    {reply, Result, NewState}.


handle_cast({clear, MaxCount}, State) ->
    {noreply, State #state{start_time = os:timestamp(), count = 0, end_time = undefined, max_count = MaxCount}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
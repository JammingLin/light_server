%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 八月 2014 17:28
%%%-------------------------------------------------------------------
-module(datetime_mock).
-author("long").

%% API
-export([start_link/0, stop/0, init/1, handle_call/3]).
-export([date/0, time/0, localtime/0, timestamp/0]).

-define(SERVER, ?MODULE).

-record(state, {terms}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

init([]) ->
    FileName = record_file:get_datetime_record_name(),
    {ok, Terms} = file:consult(FileName),
    {ok, #state{terms = Terms}}.

date() ->

    gen_server:call(?SERVER, get_date).

time() ->
    gen_server:call(?SERVER, get_time).

localtime() ->
    gen_server:call(?SERVER, get_localtime).

timestamp() ->
    gen_server:call(?SERVER, get_timestamp).

handle_call(get_date, _From, #state{terms=Terms} = State) ->
    [#{year:=_Y, month:=_M, day:=_D}=H | Rest] = Terms,
    {reply, H, State#state{terms=Rest}};
handle_call(get_time, _From, #state{terms=Terms} = State) ->
    [#{hour:=_H, minute:=_M, second:=_S}=H | Rest] = Terms,
    {reply, H, State#state{terms=Rest}};
handle_call(get_localtime, _From, #state{terms=Terms} = State) ->
    [H | Rest] = Terms,
    #{year:=_, month:=_, day:=_, hour:=_, minute:=_, second:=_} = H,
    {reply, H, State#state{terms=Rest}};
handle_call(get_timestamp, _From, #state{terms=Terms} = State) ->
    [{timestamp, TimeStamp} | Rest] = Terms,
    {reply, TimeStamp, State#state{terms=Rest}}.


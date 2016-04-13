-module(db_ets).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("db_ets.hrl").
-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get/2, select/2]).
-export([save/2]).
-export([delete/2]).
-export([delete_match/2]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(Table, Key)->
    gen_server:call(?SERVER, {get, Table, Key}).

select(Table, MatchSpec)->
    gen_server:call(?SERVER, {select, Table, MatchSpec}).

save(Table, Value)->
    gen_server:cast(?SERVER, {save, Table, Value}).

delete(Table, Key)->
    gen_server:cast(?SERVER, {delete, Table, Key}).

delete_match(Table, MatchSpec)->
    gen_server:cast(?SERVER, {delete_match, Table, MatchSpec}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    db_ets_init:init(),
    {ok, #state{}}.

handle_call({get, Table, Key}, _From, State)->
    Result = get_from_ets(Table, Key),
    {reply, Result, State};
handle_call({select, Table, MatchSpec}, _From, State)->
    Result = select_from_ets(Table, MatchSpec),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({save, Table, Value}, State)->
    lager:info("save table ~p value ~p", [Table, Value]),
    save_to_ets(Table, Value),
    save_to_db(Table, Value),
    {noreply, State};
handle_cast({delete, Table, Key}, State)->
    delete_from_ets(Table, Key),
    delete_from_db(Table, Key),
    {noreply, State};
handle_cast({delete_match, Table, MatchSpec}, State)->
    DeleteList = select_from_ets(Table, MatchSpec),
    delete_match_from_ets(Table, MatchSpec),
    delete_match_from_db(Table, DeleteList),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_from_ets(Table, Key)->
    case ets:lookup(Table, Key) of
        []-> [];
        [Record]->Record
    end.

select_from_ets(Table, MatchSpec)->
    case ets:select(Table, MatchSpec) of
        []->[];
        Results->Results
    end.

save_to_ets(_Table, _Value)->
    ok.

save_to_db(Table, Value)->
    db:save(Table, Value).

delete_from_ets(Table, Key)->
    ets:delete(Table, Key).
delete_from_db(Table, Key)->
    db:delete(Table, Key).

delete_match_from_ets(Table, MatchSpec)->
    ets:match_delete(Table, MatchSpec).

delete_match_from_db(_Table, _Records)->
    ok.

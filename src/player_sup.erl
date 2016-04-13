%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 十二月 2014 18:08
%%%-------------------------------------------------------------------
-module(player_sup).
-author("Administrator").

-behaviour(supervisor).

%% API
-export([start_link/0, start_player/4, stop_player/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {'game_player', {'game_player', start_link, []},
        Restart, Shutdown, Type, ['game_player']},

    {ok, {SupFlags, [AChild]}}.

start_player(NetPid, PlayerID, Account, Area) ->
    supervisor:start_child(?SERVER, [NetPid, PlayerID, Account, Area]).

stop_player(Node, PlayerPid) when is_pid(PlayerPid) ->
    case supervisor:terminate_child({?SERVER, Node}, PlayerPid) of
        {error, Error} ->
            lager:error("stop_player(~p, ~p), error, Error:~p)",
                [Node, PlayerPid, Error]);
        ok -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

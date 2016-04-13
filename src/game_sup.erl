-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, _} = Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    cache_manager:start(),
    gate_net:init(),
    Result.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    rsa:init(),
%%     DBNode = get_db_node(),
    TpltDir = get_tplt_dir(),
    {ok, LogHelperConfig} = application:get_env(game, log_helper),
    datetime:start_link(),
    ensure_dirs(),
    {RanchSupSpec, ListenerSpec} = gate_server:child_spec(),
    {ok, { {one_for_one, 5, 10},
        [
            ?CHILD(db, worker),
            ?CHILD(id_generator, worker),
            ?CHILD(tplt_data, worker, [TpltDir]),
            ?CHILD(cache, worker, [get_cache_port()]),
            ?CHILD(m_rand, worker),
            ?CHILD(monitor_nodes, worker),
            ?CHILD(log_helper, worker, [game_log_helper, LogHelperConfig]),
            RanchSupSpec,
            ListenerSpec
        ]
    }
    }.

get_tplt_dir() ->
    case application:get_env(game, tplt_dir) of
        undefined -> "data/template/";
        {ok, TpltDir} -> TpltDir
    end.

get_cache_port() ->
    case application:get_env(game, cache_port) of
        undefined -> 8002;
        {ok, Port} -> Port
    end.

ensure_dirs() ->
    ok.

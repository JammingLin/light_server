-module(db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_SHUTDOWN(I, Type, Shutdown), {I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SqlPath = get_sql_dir(),
    lager:info("sqlPath:~p", [SqlPath]),
    DbModule = db_helper:get_db_module(),
    {ok, LogHelperConfig} = application:get_env(db, log_helper),
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(db_helper, worker, [SqlPath]),
        ?CHILD(log_helper, worker, [db_log_helper, LogHelperConfig]),
        ?CHILD_SHUTDOWN(DbModule, worker, 60 * 60 * 1000)]}
    }.

get_sql_dir() ->
    case application:get_env(db, sqldir) of
        undefined -> "data/sql/";
        {ok, SqlDir} -> SqlDir
    end.


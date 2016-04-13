%% Settings (defaults in include/emysql.hrl):
%% default_timeout (TIMEOUT = 8000)
%% lock_timeout (LOCK_TIMEOUT = 5000)

{application, emysql, [
    {description, "Emysql - Erlang MySQL driver"},
    {vsn, "0.4.1"},
    {modules, []}, 
    {mod, {emysql_app, ["%MAKETIME%"]}},
    {registered, [emysql_conn_mgr, emysql_sup]},
    {applications, [kernel, stdlib, crypto]},
    {env, [
		{host, "127.0.0.1"},
		{port, 3306},
		{user, "root"},
		{password, ""},
		{database, "knowledge"},
		{default_timeout, 5000},
        {conn_test_period, 28000}]}
]}.

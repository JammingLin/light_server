%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:57
%%%-------------------------------------------------------------------
-module(db_mysql).
-author("long").

%% API
-export([start/1, stop/0, query/1, query/2, execute/1, execute/2, execute2/1, execute2/2]).
-export([to_map/1]).

start(SqlPath) ->
    db_mysql_common:start(),
    {ok, [Pool|_]} = application:get_env(db, db_pools),
    db_update:start([Pool], SqlPath).

stop() ->
    ok.

query(Query) ->
    db_mysql_common:query(Query).

query(_Key, Query) ->
    db_mysql_common:query(Query).

execute(Query) ->
    db_mysql_common:execute(Query).

execute(_Key, Query) ->
    db_mysql_common:execute(Query).

execute2(Query) ->
    db_mysql_common:execute2(Query).

execute2(_Key, Query) ->
    db_mysql_common:execute2(Query).

to_map(Result) ->
    db_mysql_common:to_map(Result).
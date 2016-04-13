%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 19:57
%%%-------------------------------------------------------------------
-module(emysql_helper).
-author("long").

-include_lib("emysql.hrl").

%% API
-export([query/2, query/3, execute/2, execute/3, execute2/2, execute2/3]).
-export([to_map/1]).


query(PoolName, Query) ->
    Result = emysql:execute(PoolName, Query),
    Records = to_map(Result),
    Records.

query(PoolName, Query, Args) ->
    Result = emysql:execute(PoolName, Query, Args),
    Records = to_map(Result),
    Records.

execute(PoolName, Query) ->
    Result = execute2(PoolName, Query),
    case Result of
        #ok_packet{affected_rows = Rows} ->
            Rows;
        _ ->
            Result
    end.

execute(PoolName, Query, Args) ->
    Result = execute2(PoolName, Query, Args),
    case Result of
        #ok_packet{affected_rows = Rows} ->
            Rows;
        #error_packet{}=Error ->
			throw({mysql_error, Error})
    end.

execute2(PoolName, Query) ->
    emysql:execute(PoolName, Query).

execute2(PoolName, Query, Args) ->
    emysql:execute(PoolName, Query, Args).

to_map(Result = #result_packet{}) ->
    FieldList = Result#result_packet.field_list,
    KVList = get_kv_list(FieldList, Result#result_packet.rows),
    Records = [maps:from_list(L) || L <- KVList],
    Records.

get_kv_list(FieldList, ValueList) ->
    [get_kv_list(FieldList, Record, []) || Record <- ValueList].

get_kv_list([], [], KVList) ->
    KVList;
get_kv_list([#field{name=Name}|FieldRest], [Value | ValueRest], KVList) ->
    Name1 = erlang:binary_to_atom(Name, utf8),
    get_kv_list(FieldRest, ValueRest, [{Name1, translate:from_mysql(Value)} | KVList]).
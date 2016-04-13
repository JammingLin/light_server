%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 八月 2014 10:41
%%%-------------------------------------------------------------------
-module(translate).
-author("Administrator").

-include_lib("emysql.hrl").

%% API
-export([from_mysql/1, to_mysql/1, from_cache/2, to_cache/1, translate_type/1]).
-export([to_record/2, to_map/2]).
-export([to_keyvalue_list/2]).

%% 转换来自mysql的数据
from_mysql({date, {Year, Month, Day}}) ->
    datetime:make_date(Year, Month, Day);
from_mysql({time, {Hour, Minute, Second}}) ->
    datetime:make_time(Hour, Minute, Second);
from_mysql({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    datetime:make_datetime(Year, Month, Day, Hour, Minute, Second);
from_mysql(Value) ->
    Value.

to_mysql(#{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second}) ->
    lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]);
to_mysql(#{year:=Year, month:=Month, day:=Day}) ->
    lists:concat([Year, "-", Month, "-", Day]);
to_mysql(#{hour:=Hour, minute:=Minute, second:=Second}) ->
    lists:concat([Hour, ":", Minute, ":", Second]);
to_mysql({date, {Year, Month, Day}}) ->
    lists:concat([Year, "-", Month, "-", Day]);
to_mysql({time, {Hour, Minute, Second}}) ->
    lists:concat([Hour, ":", Minute, ":", Second]);
to_mysql({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]);
to_mysql(false) ->
	0;
to_mysql(true) ->
	1;
to_mysql(Value) ->
    Value.

%% 转换来自redis的数据
from_cache(int, Value) ->
    binary_to_integer(Value);
from_cache(tinyint, <<"false">>) ->
	false;
from_cache(tinyint, <<"true">>) ->
	true;
from_cache(tinyint, Value) ->
    binary_to_integer(Value);
from_cache(smallint, Value) ->
    binary_to_integer(Value);
from_cache(mediumint, Value) ->
    binary_to_integer(Value);
from_cache(bigint, Value) ->
    binary_to_integer(Value);
from_cache(float, Value) ->
    binary_to_float(Value);
from_cache(double, Value) ->
    binary_to_float(Value);
from_cache(decimal, Value) ->
    binary_to_float(Value);
from_cache(date, Value) ->
    erlang:binary_to_term(Value);
from_cache(time, Value) ->
    erlang:binary_to_term(Value);
from_cache(datetime, Value) ->
    erlang:binary_to_term(Value);
from_cache(timestamp, Value) ->
    binary_to_integer(Value);
from_cache(year, Value) ->
    binary_to_integer(Value);
from_cache(set, _Value) ->
    throw({error, unsupport_type});
from_cache(enum, _Value) ->
    throw({error, unsupport_type});
from_cache(_Type, Value) ->
    Value.

%%把数据转换后交给redis保存
to_cache(Value) when is_tuple(Value) ->
    erlang:term_to_binary(Value);
to_cache(Value) when is_list(Value) ->
    list_to_binary(Value);
to_cache(Value) ->
    Value.

translate_type(Type) ->
    [T|_] = binary:split(Type, <<"(">>),
    binary_to_atom(T, utf8).

%% 把map中的数据, 转换成对应的record
to_record(RecordName, Map) when is_atom(RecordName), is_map(Map) ->
    F = fun({_Idx, _, Type, Field, _}, {Index, Record}) ->
        case Type of
            {list,{struct,{game_types,Struct}}} ->
                Value = maps:get(Field, Map, undefined),
                StructValue = [to_record(Struct, V) || V <- Value],
                {Index + 1, erlang:append_element(Record, StructValue)};
            {struct, {'game_types', Struct}} ->
                Value = maps:get(Field, Map, undefined),
                StructValue = case is_map(Value) of
                    true ->
                        to_record(Struct, Value);
                    false ->
                        Value
                end,
                {Index + 1, erlang:append_element(Record, StructValue)};
            _ ->
                Value = maps:get(Field, Map, undefined),
                {Index + 1, erlang:append_element(Record, Value)}
        end
    end,
    {struct, RecordInfo} = game_types:struct_info_ext(RecordName),
    {_, Record} = lists:foldl(F, {1, {RecordName}}, RecordInfo),
    Record;
to_record(RecordName, List) when is_atom(RecordName), is_list(List) ->
    [to_record(RecordName, Map) || Map <- List].

%% 把record中的数据转换成map
to_map(RecordInfo, Record) ->
    F = fun(Field, {Index, Map}) ->
        Value = element(Index, Record),
        {Index+1, maps:put(Field, Value, Map)}
    end,
    {_, Map} = lists:foldl(F, {2, #{}}, RecordInfo),
    Map.

%% 把redis 返回HashList转成 list
to_keyvalue_list(HashList, F) ->
    to_keyvalue_list(HashList, [], F).
to_keyvalue_list([], Ret, _F) ->
    Ret;
to_keyvalue_list([Key, Value | Rest], Ret, F) ->
    to_keyvalue_list(Rest, [F(Key, Value) | Ret], F).

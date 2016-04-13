%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2015 下午5:34
%%%-------------------------------------------------------------------
-module(db_record_check).
-author("jie").

%% API
-export([check/2]).

%% 检查记录的值是否和定义的类型一致, 值的范围是否在指定的区间中
check(TableName, Record) ->
    Columns = db_proxy:get_table_schema(TableName),
    lists:foreach(fun({ColumnName, Type}) ->
        Value = maps:get(ColumnName, Record),
        ok = check_type(Type, ColumnName, Value)
        end, Columns).

check_type(int, _ColumnName, Value)
    when is_integer(Value), Value >= -2147483648, Value =< 4294967295 ->
    ok;
check_type(bigint, _ColumnName, Value)
    when is_integer(Value), Value >= -9233372036854775808, Value =< 18446744073709551615 ->
    ok;
check_type(tinyint, _ColumnName, Value)
    when is_integer(Value), Value >= -128, Value =< 255 ->
    ok;
check_type(smallint, _ColumnName, Value)
    when is_integer(Value), Value >= -32768, Value =< 65535 ->
    ok;
check_type(mediumint, _ColumnName, Value)
    when is_integer(Value), Value >= -8388608, Value =< 16777215 ->
    ok;
check_type(float, _ColumnName, Value)
    when is_float(Value), Value >= -3.402823466E+38, Value =< 3.402823466E+38->
    ok;
check_type(double, _ColumnName, Value)
    when is_float(Value), Value >= -1.7976931348623157E+308, Value =< 1.7976931348623157E+308->
    ok;
check_type(decimal, _ColumnName, Value) when is_float(Value) ->
    ok;
check_type(timestamp, _ColumnName, Value)
    when is_integer(Value), Value >= 0 ->
    ok;
check_type(year, _ColumnName, Value)
    when is_integer(Value), year >= 0 ->
    ok;

check_type(char, ColumnName, Value) ->
    ok = check_string(ColumnName, char, Value, 255);

check_type(varchar, ColumnName, Value) ->
    ok = check_string(ColumnName, varchar, Value, 65535);

check_type(tinyblob, ColumnName, Value) ->
    ok = check_string(ColumnName, tinyblob, Value, 255);

check_type(tinytext, ColumnName, Value) ->
    ok = check_string(ColumnName, tinytext, Value, 255);

check_type(blob, ColumnName, Value) ->
    ok = check_string(ColumnName, blob, Value, 65535);

check_type(text, ColumnName, Value) ->
    ok = check_string(ColumnName, text, Value, 65535);

check_type(mediumblob, ColumnName, Value) ->
    ok = check_string(ColumnName, mediumblob, Value, 16777215);

check_type(mediumtext, ColumnName, Value) ->
    ok = check_string(ColumnName, mediumtext, Value, 16777215);

check_type(longblob, ColumnName, Value) ->
    ok = check_string(ColumnName, longblob, Value, 4294967295);

check_type(longtext, ColumnName, Value) ->
    ok = check_string(ColumnName, longtext, Value, 4294967295);

check_type(date, _ColumnName, #{year:=Year, month:=Month, day:=Day})
    when Year >= 0, Month >=1, Month =< 12, Day >=1, Day =< 31 ->
    ok;
check_type(time, _ColumnName, #{hour:=H, minute:=M, second:=S})
    when H >= 0, H =< 24, M >=0, M =<60, S >= 0, S =< 60 ->
    ok;
check_type(datetime, _ColumnName, #{year:=Year, month:=Month, day:=Day, hour:=Hour, minute:=Minute, second:=Second})
    when Year >= 0, Month >=1, Month =< 12, Day >=1, Day =< 31,
         Hour >= 0, Hour =< 24, Minute >=0, Minute =<60, Second >= 0, Second =< 60 ->
    ok;
check_type(Type, ColumnName, Value)->
    {mismatch, ColumnName, Value, Type}.

check_string(ColumnName, Type, Value, MaxLen) when is_binary(Value) ->
    check_string_len(ColumnName, Type, Value, iolist_size(Value), MaxLen);
check_string(ColumnName, Type, Value, MaxLen) when is_list(Value) ->
    check_string_len(ColumnName, Type, Value, length(Value), MaxLen).

check_string_len(_ColumnName, _Type, _Value, Len, MaxLen) when Len >= 0, Len =< MaxLen ->
    ok;
check_string_len(ColumnName, Type, Value, Len, MaxLen) ->
    {mismatch_length, ColumnName, Value, Type, Len, MaxLen}.
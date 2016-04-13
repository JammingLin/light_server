%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2014 14:09
%%%-------------------------------------------------------------------
-module(trans).
-author("Administrator").


%% 数据同步加锁, 避免并发修改下数据不一致

%% API
-export([execute/3, execute/4, nolock_execute/1, is_transaction/0]).
-export([
    record_cache_save/3,
    record_cache_delete/3,
    record_cache_delete/4
]).
-export([merge/3, merge/4]).

-record(trans, {count = 0, %% 事务的数量, 每次有嵌套事务, 该值都会+1, 当事务结束, 该值会自动-1
    trans_list = []}).

%% 事务操作, 有锁机制, 性能较差, Fun运行出错, 会回滚cache中的数据
%% TableName: 表名, PrimaryValue: 主键值, Fun: 事务函数
execute(TableName, PrimaryValue, Fun) when is_atom(TableName), is_function(Fun)->
    Id = {{TableName, PrimaryValue}, self()},
    %% TODO: -hidden参数，就不能用这个global:trans/2函数了, 应该用global:trans/4, 自己指定Nodes
    execute_impl(fun() -> global:trans(Id, Fun) end).

%% 事务操作, 有锁机制, 性能较差, Fun运行出错, 会回滚cache中的数据
%% TableName: 表名, PrimaryValue1:主键值1, PrimaryValue2:主键值2, Fun: 事务函数
execute(TableName, PrimaryValue1, PrimaryValue2, Fun)  when is_atom(TableName), is_function(Fun)->
    Id = {{TableName, PrimaryValue1, PrimaryValue2}, self()},
    execute_impl(fun() -> global:trans(Id, Fun) end).

%% 事务操作, 但是没有锁机制, 性能较高, Fun运行出错, 会回滚cache中的数据
nolock_execute(Fun) when is_function(Fun)->
    execute_impl(Fun).

execute_impl(TransFun) ->
    case get(transaction) of
        undefined ->
            Trans1 = #trans{count=1},
            put(transaction, Trans1);
        Trans ->
            Trans1 = Trans#trans{count=Trans#trans.count+1},
            put(transaction, Trans1)
    end,
    try
        Result = TransFun(),
        commit(),
        Result
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        erlang:raise(Class, Reason, Stacktrace)
    after
        Trans3 = get(transaction),
        case Trans3#trans.count of
            1 -> erase(transaction);
            Count -> put(transaction, Trans3#trans{count=Count-1})
        end
    end.

is_transaction() ->
    get(transaction) =/= undefined.

%% 把真实的数据和事务中的临时数据进行合并
merge(TableName, PrimaryValue, Result) when is_atom(TableName) ->
    case db:get_primary_name(TableName) of
        [_PrimaryName] ->
            merge_one(TableName, PrimaryValue, Result);
        [PrimaryName1, PrimaryName2] ->
            merge_all(TableName, PrimaryName1, PrimaryName2, PrimaryValue, Result)
    end.

%% 把真实的数据和事务中的临时数据进行合并
merge(TableName, PrimaryValue1, PrimaryValue2, Result) when is_atom(TableName)->
    Trans = get(transaction),
    F = fun(Table, Fun, RecordPrimaryKey) ->
        case Fun of
            no_trans_delete ->
                case RecordPrimaryKey of
                    [PV] ->
                        PV =:= PrimaryValue1;
                    [PV1, PV2] ->
                        PV1 =:= PrimaryValue1 andalso PV2 =:= PrimaryValue2
                end;
            no_trans_save ->
                [Record] = RecordPrimaryKey,
                [PrimaryName1, PrimaryName2] = db:get_primary_name(Table),
                V1 = maps:get(PrimaryName1, Record),
                V2 = maps:get(PrimaryName2, Record),
                V1 =:= PrimaryValue1 andalso V2 =:= PrimaryValue2
        end
    end,
    merge_impl(Trans, TableName, F, Result).

merge_one(TableName, PrimaryValue, Result) when is_atom(TableName), Result == [] orelse is_map(Result) ->
    Trans = get(transaction),
    F = fun(Table, Fun, RecordPrimaryKey) ->
        case Fun of
            no_trans_delete ->
                PV = hd(RecordPrimaryKey),
                PV =:= PrimaryValue;
            no_trans_save ->
                [Record] = RecordPrimaryKey,
                [PrimaryName] = db:get_primary_name(Table),
                Value = maps:get(PrimaryName, Record),
                Value =:= PrimaryValue
        end
    end,
    merge_impl(Trans, TableName, F, Result).

merge_impl(Trans, TableName, F, Result) ->
    MemoryList = [{Fun, Table, Rest} || #{function:=Fun, args:=[Table | Rest]} <- Trans#trans.trans_list,
        TableName =:= Table, F(Table, Fun, Rest)],
    case MemoryList of
        [] -> Result;
        [{no_trans_delete, _Table, _Record} | _] ->
            %% 如果优先遇到delete的情况, 应该返回空值, 这个事务列表是倒序的, 越后面的操作, 排在越前面
            [];
        [{no_trans_save, _Table, [Record]} | _] ->
            Record
    end.

%% 当表是双主键的时候, 只指定第一个主键的时候, 获取这个主键里的所有对应的记录
merge_all(TableName, PrimaryName1, PrimaryName2, PrimaryValue, ResultList) when is_atom(TableName), is_list(ResultList)->
    Trans = get(transaction),
    TransList = [MFA || #{args:=[Table | _]}=MFA <- Trans#trans.trans_list, TableName =:= Table],
    get_from_record_list(TransList, ResultList, PrimaryName1, PrimaryName2, PrimaryValue).

get_from_record_list(TransList, ResultList, PrimaryName1, PrimaryName2, PrimaryValue) ->
    case TransList of
        [] -> ResultList;
        _ ->
            MemoryList1 = lists:reverse(TransList),
            case merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, MemoryList1, ResultList) of
                [] ->[];
                FinalList -> FinalList
            end
    end.

merge_two_primary(_PrimaryName1, _PrimaryName2, _PrimaryValue, [], RecordList) ->
    RecordList;
merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, [#{function:=no_trans_delete, args:=[_Table, V1]} | Rest], RecordList) ->
    case PrimaryValue == V1 of
        true ->
            merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, Rest, []);
        false ->
            merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, Rest, RecordList)
    end;
merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, [#{function:=no_trans_delete, args:=[_Table, V1, V2]} | Rest], RecordList) ->
    Pred = fun(Data) ->
        PrimaryValue == V1 andalso maps:get(PrimaryName2, Data) == V2
    end,
    RecordList1 = delete(Pred, RecordList, []),
    merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, Rest, RecordList1);
merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, [#{function:=no_trans_save, args:=[_Table, Record]} | Rest], RecordList) ->
    V1 = maps:get(PrimaryName1, Record),
    case PrimaryValue =:= V1 of
        true ->
            V2 = maps:get(PrimaryName2, Record),
            Pred = fun(R) ->
                maps:get(PrimaryName2, R) =:= V2
            end,
            DataList1 = delete(Pred, RecordList, []),
            DataList2 = [Record | DataList1],
            merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, Rest, DataList2);
        false ->
            merge_two_primary(PrimaryName1, PrimaryName2, PrimaryValue, Rest, RecordList)
    end.

delete(_Pred, [], Result) ->
	Result;
delete(Pred, [E | Tail], Result) ->
	case Pred(E) of
		true -> delete(Pred, Tail, Result);
		false -> delete(Pred, Tail, [E | Result])
	end.

%% 对save操作进行纪录, 用于还原操作
record_cache_save(Module, TableName, Record) when is_map(Record)->
    record_impl(Module, no_trans_save, [TableName, Record]).

%% 对delete操作进行纪录, 用于异常时候的还原操作
record_cache_delete(Module, Table, PrimaryValue) ->
    record_impl(Module, no_trans_delete, [Table, PrimaryValue]).
record_cache_delete(Module, Table, PrimaryValue1, PrimaryValue2) ->
    record_impl(Module, no_trans_delete, [Table, PrimaryValue1, PrimaryValue2]).

record_impl(Module, Fun, Args) ->
    case get(transaction) of
        undefined -> non_trans;
        Trans ->
            L = [#{module=>Module, function=>Fun, args=>Args} | Trans#trans.trans_list],
            put(transaction, Trans#trans{trans_list=L})
    end.

commit() ->
    case get(transaction) of
        undefined -> ok;
        #trans{count=Count, trans_list=List} ->
            case Count of
                1 ->
                    L = lists:reverse(List),
                    [apply(Module, Fun, Args) || #{module:=Module, function:=Fun, args:=Args} <- L];
                _ -> ok
            end
    end.
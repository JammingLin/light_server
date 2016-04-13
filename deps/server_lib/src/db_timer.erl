%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%  定时保存数据到数据库中
%%% @end
%%% Created : 31. Mar 2015 下午5:50
%%%-------------------------------------------------------------------
-module(db_timer).
-author("jie").

-export([start_link/0, stop/0, stop_when_db_error/0]).
-export([get/2, get/3]).
-export([save/2]).
-export([delete/2, delete/3, delete_all/1]).
-export([save_to_db_for_test/0, start_timer_for_test/2]).

-include_lib("stdlib/include/ms_transform.hrl").

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([test/0, test_stress/0, test_ets/0, test_save_cost_time/0]).

-define(TIME_TO_SAVE, 5 * 60 * 1000). %% 设定每5分钟保存一次数据库
-define(TIME_TO_HOLD_STORE, 60 * 60 * 1000). %% 让数据落地最多只能保存一个小时, 否则时间太长了

-record(state, {index=0, ets_list=[], timer}).

start_link() ->
    start_link(?TIME_TO_SAVE).

start_link(TimeToSave) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [TimeToSave], []).

stop() ->
    lager:info("cache data save to database......"),
    gen_server:call(?MODULE, stop, ?TIME_TO_HOLD_STORE).

stop_when_db_error() ->
    gen_server:call(?MODULE, stop_when_db_error, ?TIME_TO_HOLD_STORE).


-spec get(TableName::atom(), PrimaryValue::any()) -> map().
get(TableName, PrimaryValue) when is_atom(TableName) ->
    QueueCache = get_from_queue(TableName, PrimaryValue),
    DBRecords = db_real:get(TableName, PrimaryValue),
    merge(TableName, QueueCache, DBRecords).

-spec get(TableName::atom(), PrimaryValue1::any(), PrimaryValue2::any()) -> [map()].
get(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    QueueCache = get_from_queue(TableName, PrimaryValue1, PrimaryValue2),
    DBRecords = db_real:get(TableName, PrimaryValue1, PrimaryValue2),
    merge(TableName, QueueCache, DBRecords).

%% 定时保存到数据库
-spec save(TableName :: atom(), Record :: map()) -> Result | [Result].
save(TableName, Record) when is_atom(TableName) ->
    case db:get_primary_name(TableName) of
        [PrimaryName] ->
            PrimaryValue = maps:get(PrimaryName, Record),
            gen_server:cast(?MODULE, {save, TableName, PrimaryValue, Record});
        [PrimaryName1, PrimaryName2] ->
            PrimaryValue1 = maps:get(PrimaryName1, Record),
            PrimaryValue2 = maps:get(PrimaryName2, Record),
            gen_server:cast(?MODULE, {save, TableName, PrimaryValue1, PrimaryValue2, Record})
    end.

%% 定时删除数据
-spec delete(TableName :: atom(), Record :: map()) -> Result | [Result].
delete(TableName, PrimaryValue) when is_atom(TableName) ->
    gen_server:cast(?MODULE, {delete, TableName, PrimaryValue}).

delete(TableName, PrimaryValue1, PrimaryValue2) when is_atom(TableName) ->
    gen_server:cast(?MODULE, {delete, TableName, PrimaryValue1, PrimaryValue2}).

delete_all(TableName) when is_atom(TableName) ->
    gen_server:cast(?MODULE, {delete_all, TableName}).

%% 该函数主要是为了方便测试用, 正式环境不用
save_to_db_for_test() ->
    gen_server:call(?MODULE, save_to_db_for_test, infinity).

start_timer_for_test(Pid, TimeToSave) ->
    start_timer(Pid, TimeToSave).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([TimeToSave]) ->
    process_flag(trap_exit, true),

    restore_etsdata(),

    {ok, _} = db_timer_save:start_link(),
    Tid1 = ets:new(db_timer_queue_1, [public]),
    Tid2 = ets:new(db_timer_queue_2, [public]),
    %% 设定定时器, 定时保存
    Timer = start_timer(self(), TimeToSave),
    {ok, #state{index=0, ets_list=[Tid1, Tid2], timer=Timer}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(stop_when_db_error, _From, #state{ets_list=[Tid1, Tid2]}=State) ->
    store_etsdata(Tid1, Tid2),
    {stop, db_error, ok, State};

handle_call({get, TableName, PrimaryValue}, _From, #state{ets_list=[Tid1, Tid2]}=State) ->
    Key = {TableName, PrimaryValue},
    L1Ets = lookup_data(Tid1, Key),
    L2Ets = lookup_data(Tid2, Key),

    {reply, {L1Ets, L2Ets}, State};

%% 该函数主要是为了方便测试用, 正式环境不用
handle_call(save_to_db_for_test, _From,  #state{ets_list = EtsList}=State) ->
    Tid = get_current_ets(EtsList),
    db_timer_save:sync_save_to_db(Tid),
    {reply, ok, State};

%% 该函数主要是为了方便测试用, 正式环境不用
handle_call(swap_for_test, _From, #state{ets_list=EtsList}=State) ->
    EtsList1 = swap_ets(EtsList),
    io:format("swap for test,Tid:~p~n~n", [EtsList1]),
    {reply, get_current_ets(EtsList1), State#state{ets_list=EtsList1}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({delete_all, TableName}, #state{ets_list=[Tid1, Tid2]}=State) ->
    DelFun = fun(Tid) ->
        ets:foldl(
            fun({{Table, _PrimaryValue}=Key, _L}, Acc) ->
                case Table == TableName of
                    true -> ets:delete(Tid, Key);
                    false -> ok
                end,
                Acc
            end, [], Tid)
             end,
    DelFun(Tid1),
    DelFun(Tid2),
    db_real:delete_all(TableName),
    {noreply, State};

handle_cast({save, TableName, PrimaryValue, Record}, #state{ets_list = EtsList, index=Index}=State) ->
    Tid = get_current_ets(EtsList),
    Key = {TableName, PrimaryValue},
    L = case ets:lookup(Tid, Key) of
            [{_Key, List}] -> List;
            [] -> []
        end,
    L1 = lists:keydelete(save, 1, L),
    L2 = [{save, Index, Record} | L1],
    ets:insert(Tid, {Key, L2}),
    {noreply, State#state{index=Index+1}};

handle_cast({save, TableName, PrimaryValue1, PrimaryValue2, Record}, #state{ets_list = EtsList, index=Index}=State) ->
    Tid = get_current_ets(EtsList),
    Key = {TableName, PrimaryValue1},
    L = case ets:lookup(Tid, Key) of
            [{_Key, List}] -> List;
            [] -> []
        end,
    L1 = lists:keydelete(PrimaryValue2, 3, L),
    L2 = [{save, Index, PrimaryValue2, Record} | L1],
    ets:insert(Tid, {Key, L2}),
    {noreply, State#state{index=Index+1}};

handle_cast({delete, TableName, PrimaryValue}, #state{ets_list = EtsList, index=Index}=State) ->
    Tid = get_current_ets(EtsList),
    Key = {TableName, PrimaryValue},
    L1 = [{delete, Index}],
    ets:insert(Tid, {Key, L1}),
    {noreply, State#state{index=Index+1}};

handle_cast({delete, TableName, PrimaryValue1, PrimaryValue2}, #state{ets_list = EtsList, index=Index}=State) ->
    Tid = get_current_ets(EtsList),
    Key = {TableName, PrimaryValue1},
    L = case ets:lookup(Tid, Key) of
            [{_Key, List}] -> List;
            [] -> []
        end,
    L1 = lists:keydelete(PrimaryValue2, 3, L),
    L2 = [{delete, Index, PrimaryValue2} | L1],
    ets:insert(Tid, {Key, L2}),
    {noreply, State#state{index=Index+1}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, _Timer, {save_data_to_db, TimeToSave}}, #state{ets_list=EtsList}=State) ->
    EtsList1 = case db_timer_save:can_save() of
        true ->
            Tid = get_current_ets(EtsList),
            EL = swap_ets(EtsList),
            db_timer_save:async_save_to_db(Tid),
            EL;
        false ->
            EtsList
    end,
    Timer1 = start_timer(self(), TimeToSave),
    {noreply, State#state{ets_list = EtsList1, timer=Timer1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ets_list = [Tid1, Tid2], timer=Timer}=_State) ->
    lager:info("terminate:~p", [_Reason]),
    erlang:cancel_timer(Timer),

    ok = db_timer_save:sync_save_to_db(Tid2),
    ok = db_timer_save:sync_save_to_db(Tid1),

    db_timer_save:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------内部函数--------------------------------------------

lookup_data(Tid, Key) ->
    case ets:lookup(Tid, Key) of
        [] -> [];
        [{Key, L}] -> L
    end.

do_get(_Key, [], Acc) ->
    Acc;
do_get(Key, [{delete, Index} | _Rest], Acc) ->
    [{Key, delete, Index} | Acc];
do_get(Key, [{delete, Index, PV2} | Rest], Acc)->
    Acc1 = [{Key, delete, Index, PV2} | Acc],
    do_get(Key, Rest, Acc1);
do_get(Key, [{save, Index, Record} | Rest], Acc)->
    Acc1 = [{Key, save, Index, Record} | Acc],
    do_get(Key, Rest, Acc1);
do_get(Key, [{save, Index, PV2, Record} | Rest], Acc)->
    Acc1 = [{Key, save, Index, PV2, Record} | Acc],
    do_get(Key, Rest, Acc1).

do_get(_PrimaryValue2, _Key, [], Acc) ->
    Acc;
do_get(_PrimaryValue2, Key,[{delete, Index} | _Rest], Acc) ->
    [{Key, delete, Index} | Acc];
do_get(PrimaryValue2, Key, [{delete, Index, PV2} | _Rest], _Acc) when PrimaryValue2 == PV2->
    [{Key, delete, Index, PV2}];
do_get(PrimaryValue2, Key, [{save, Index, PV2, Record} | _Rest], _Acc) when PrimaryValue2 == PV2->
    [{Key, save, Index, PV2, Record}];
do_get(PrimaryValue2, Key, [_ | Rest], Acc) ->
    do_get(PrimaryValue2, Key,Rest, Acc).

start_timer(Pid, TimeToSave) ->
    erlang:start_timer(TimeToSave, Pid, {save_data_to_db, TimeToSave}).

%% 两个ets交替使用
swap_ets([CurrTid, Tid]) ->
    [Tid, CurrTid].

get_current_ets([CurrTid, _Tid]) ->
    CurrTid.

get_from_queue(TableName, PrimaryValue) ->
    {L1, L2} = gen_server:call(?MODULE, {get, TableName, PrimaryValue}),
    Key = {TableName, PrimaryValue},
    L = do_get(Key, L1 ++ L2, []),
    lists:keysort(3, L).

get_from_queue(TableName, PrimaryValue1, PrimaryValue2) ->
    {L1, L2} = gen_server:call(?MODULE, {get, TableName, PrimaryValue1}),
    Key = {TableName, PrimaryValue1},
    L = do_get(PrimaryValue2, Key, L1 ++ L2, []),
    lists:keysort(3, L).

merge(Table, QueueCache, TotalRecords) ->
    merge_impl(db:get_primary_name(Table), QueueCache, TotalRecords).

merge_impl(_PrimaryName, [], TotalRecords) ->
    TotalRecords;
merge_impl(PrimaryName, [{{_TableName, _PV}, delete, _Index} | Rest], _TotalRecords) ->
    merge_impl(PrimaryName, Rest, []);
merge_impl([PrimaryName1, PrimaryName2], [{{_TableName, _PV1}, delete, _Index, PV2} | Rest], TotalRecords) ->
    TotalRecords1 = delete_member(PrimaryName2, PV2, TotalRecords),
    merge_impl([PrimaryName1, PrimaryName2], Rest, TotalRecords1);
merge_impl([PrimaryName1], [{{_TableName, V}, save, _Index, Record} | Rest], TotalRecords) ->
    TotalRecords1 = [Record | delete_member(PrimaryName1, V, TotalRecords)],
    merge_impl([PrimaryName1], Rest, TotalRecords1);
merge_impl([PrimaryName1, PrimaryName2], [{{_TableName, _V1}, save, _Index, V2, Record} | Rest], TotalRecords) ->
    TotalRecords1 = [Record | delete_member(PrimaryName2, V2, TotalRecords)],
    merge_impl([PrimaryName1, PrimaryName2], Rest, TotalRecords1).

delete_member(PrimaryName, V, DBRecords) ->
    [R || R <- DBRecords, maps:get(PrimaryName, R) =/= V].

etsdata_to_file(Tid, Id) ->
    FileName = get_etsdata_filename(Id),
    ok = ets:tab2file(Tid, FileName).

get_etsdata_filename(Id) when is_integer(Id)->
    lists:concat(["./db_timer_fail_etsdata", Id, ".dat"]).

etsdata_from_file(Id) ->
    FileName = get_etsdata_filename(Id),
    case ets:file2tab(FileName) of
        {'ok', Tab} ->
            {ok, FileName, Tab};
        {error, {read_error, {file_error, _FilePath, enoent}}} ->
            %% file not exist
            lager:info("etsdata_from_file nothing to restore, filename:~p.", [FileName]),
            {ok, empty, empty}
    end.

store_etsdata(Tid1, Tid2) ->
    etsdata_to_file(Tid1, 1),
    ets:delete_all_objects(Tid1),

    etsdata_to_file(Tid2, 2),
    ets:delete_all_objects(Tid2),
    ok.

restore_etsdata() ->
    %% 先2再1,因为Tid2里的数据比Tid1里的早
    restore_etsdata(2),
    restore_etsdata(1).

restore_etsdata(Id) ->
    case etsdata_from_file(Id) of
        {ok, empty, empty} ->
            ok;
        {ok, FileName, TabId} when is_integer(TabId) ->
            ok = db_timer_save:sync_save_to_db(TabId),
            ok = file:delete(FileName),
            true = ets:delete(TabId),
            lager:info("restore_etsdata ~p ok.", [Id]),
            ok
    end.

%%-------------------------------------------测试代码-----------------------------------------
test() ->
    db_app:start(),
    test_delete_all(),
    test_get(),
    test_save_to_db(),
    test_save_timer(),
    test_timer_swap(),
    test_delete(),
    test_save_delete(),
    test_delete_save(),
    io:format("********test db_timer finish!********~n"),
    ok.

test_delete_all() ->
    TimeToSave = 60 * 60 * 1000, %% 这里保存直接调用save_to_db测试, 因此设置了一个很大的时间, 不用定时的方式测试
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),
    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),

    delete_all(Table1),
    [] = db_timer:get(Table1, 1),
    delete_all(Table2),
    [] = db_timer:get(Table2, 1),
    [] = db_timer:get(Table2, 1, 1),

    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 2, age => 2}),
    db_timer:delete(Table1, 1),
    db_timer:delete(Table2, 1),
    [] = db_timer:get(Table1, 1),
    [] = db_timer:get(Table2, 1),
    [] = db_timer:get(Table2, 1, 1),
    [] = db_timer:get(Table2, 1, 2),

    delete_all(Table1),
    db_timer:save(Table1, #{id => 1, age => 1}),
    db_timer:save(Table1, #{id => 2, age => 2}),
    gen_server:call(?MODULE, swap_for_test), %% 交换缓存, 测试双缓冲的情况下, 针对相同的主键, 是否有问题
    db_timer:delete(Table1, 1),
    db_timer:delete(Table1, 2),
    RR3 = #{id => 3, age => 3},
    RR4 = #{id => 4, age => 4},
    db_timer:save(Table1, RR3),
    db_timer:save(Table1, RR4),
    [] = db_timer:get(Table1, 1),
    [] = db_timer:get(Table1, 2),
    [RR3] = db_timer:get(Table1, 3),
    [RR4] = db_timer:get(Table1, 4),

    db_timer:save(Table2, #{player_id => 1, item_id => 1, age => 1}),
    db_timer:save(Table2, #{player_id => 1, item_id => 2, age => 2}),
    gen_server:call(?MODULE, swap_for_test), %% 交换缓存, 测试双缓冲的情况下, 针对相同的主键, 是否有问题
    db_timer:delete(Table2, 1, 2),
    db_timer:delete(Table2, 1),
    R3 = #{player_id => 1, item_id => 3, age => 3},
    R4 = #{player_id => 1, item_id => 4, age => 4},
    db_timer:save(Table2, R3),
    db_timer:save(Table2, R4),
    [] = db_timer:get(Table2, 1, 1),
    [] = db_timer:get(Table2, 1, 2),
    [R3] = db_timer:get(Table2, 1, 3),
    [R4] = db_timer:get(Table2, 1, 4),
    utils:check_list([R3, R4], db_timer:get(Table2, 1)),

    stop(),
    ok.

test_get() ->
    test_get_one(),
    test_get_multi().

test_get_one() ->
    TimeToSave = 60 * 60 * 1000, %% 这里保存直接调用save_to_db测试, 因此设置了一个很大的时间, 不用定时的方式测试
    start_link(TimeToSave),

    Table1 = one_primary,
    delete_all(Table1),

    R1 = #{id => 1, age => 1},
    db_real:save(Table1, R1),
    R11 = R1#{age := 2},
    db_timer:save(Table1, R11),
    [R11] = db_timer:get(Table1, 1),
    db_timer:delete(Table1, 1),
    [] = db_timer:get(Table1, 1),
    stop(),
    io:format("test_get_one finish!~n"),
    ok.

test_get_multi() ->
    TimeToSave = 60 * 60 * 1000, %% 这里保存直接调用save_to_db测试, 因此设置了一个很大的时间, 不用定时的方式测试
    start_link(TimeToSave),

    Table1 = two_primary,
    delete_all(Table1),

    R1 = #{player_id => 1, item_id => 1, age => 1},
    R2 = #{player_id => 1, item_id => 2, age => 2},
    R3 = #{player_id => 1, item_id => 3, age => 3},
    R4 = #{player_id => 1, item_id => 4, age => 4},
    [db_real:save(Table1, R) || R <- [R1, R2, R3, R4]],

    db_timer:delete(Table1, 1, 1),
    R21 = R2#{age := 21},
    db_timer:save(Table1, R21),

    R5 = #{player_id => 1, item_id => 5, age => 5},
    R6 = #{player_id => 1, item_id => 6, age => 6},
    db_timer:save(Table1, R5),
    db_timer:save(Table1, R6),
    db_timer:delete(Table1, 1, 6),
    utils:check_list([R21, R3, R4, R5], db_timer:get(Table1, 1)),
    stop(),
    io:format("test_get_multi finish!~n"),
    ok.

test_save_to_db() ->
    TimeToSave = 60 * 60 * 1000, %% 这里保存直接调用save_to_db测试, 因此设置了一个很大的时间, 不用定时的方式测试
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    db_timer:save(Table1, R1),
    R11 = R1#{age := 11},
    db_timer:save(Table1, R11),

    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    R22 = R2#{age := 3},
    db_timer:save(Table2, R2),
    db_timer:save(Table2, R22),

    save_to_db_for_test(),

    [R11] = db_real:get(Table1, ID),
    [R22] = db_real:get(Table2, PlayerID),
    stop(),
    io:format("test_save_to_db finish!~n"),
    ok.

test_save_timer() ->
    TimeToSave = 500,
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    db_timer:save(Table1, R1),

    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    db_timer:save(Table2, R2),

    timer:sleep(TimeToSave div 2),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID),

    timer:sleep(TimeToSave),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID),

    stop(),
    io:format("test_save_timer finish!~n"),
    ok.

test_timer_swap() ->
    TimeToSave = 500,
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    db_timer:save(Table1, R1),
    db_timer:save(Table2, R2),

    timer:sleep(TimeToSave + 200),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID),

    R11 = R1#{age:=2},
    R22 = R2#{age:=3},
    db_timer:save(Table1, R11),
    db_timer:save(Table2, R22),

    timer:sleep(TimeToSave + 200),
    [R11] = db_real:get(Table1, ID),
    [R22] = db_real:get(Table2, PlayerID),

    stop(),
    io:format("test_timer_swap finish!~n"),
    ok.

test_delete() ->
    TimeToSave = 500,
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},
    R3 = #{player_id => PlayerID, item_id => 3, age => 3},
    db_real:save(Table1, R1),
    db_real:save(Table2, R2),
    db_real:save(Table2, R3),

    db_timer:delete(Table1, ID),
    db_timer:delete(Table2, PlayerID, 2),
    timer:sleep(TimeToSave + 200),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID, 2),
    [R3] = db_real:get(Table2, PlayerID, 3),

    stop(),
    io:format("test_delete finish!~n"),
    ok.

test_save_delete() ->
    TimeToSave = 500,
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},

    db_timer:save(Table1, R1),
    db_timer:delete(Table1, ID),
    db_timer:save(Table2, R2),
    db_timer:delete(Table2, PlayerID),
    timer:sleep(TimeToSave + 200),
    [] = db_real:get(Table1, ID),
    [] = db_real:get(Table2, PlayerID, 2),
    [] = db_real:get(Table2, PlayerID),

    stop(),
    io:format("test_save_delete finish!~n"),
    ok.

test_delete_save() ->
    TimeToSave = 500,
    start_link(TimeToSave),
    Table1 = one_primary,
    Table2 = two_primary,
    delete_all(Table1),
    delete_all(Table2),

    ID = 1,
    R1 = #{id => ID, age => 1},
    PlayerID = 1,
    R2 = #{player_id => PlayerID, item_id => 2, age => 2},

    db_timer:delete(Table1, ID),
    db_timer:save(Table1, R1),
    db_timer:delete(Table2, PlayerID),
    db_timer:save(Table2, R2),
    timer:sleep(TimeToSave + 200),
    [R1] = db_real:get(Table1, ID),
    [R2] = db_real:get(Table2, PlayerID, 2),
    [R2] = db_real:get(Table2, PlayerID),

%%     stop(),
    io:format("test_delete_save finish!~n"),
    ok.

%% 压力测试
test_stress() ->
    dgry_server:start('dgry_db@127.0.0.1'),
    TimeToSave = 60 * 60 * 1000, %% 这里保存直接调用save_to_db测试, 因此设置了一个很大的时间, 不用定时的方式测试
    start_link(TimeToSave),
    Table1 = one_primary,
    delete_all(Table1),

    lists:foreach(fun(ID) ->
        R1 = #{id => ID, age => 1},
        db_timer:save(Table1, R1)
    end, lists:seq(1, 100000)),

    {Time, _} = timer:tc(fun save_to_db_for_test/0),
    io:format("time:~p~n", [Time]),

    stop(),
    io:format("test_stress finish!~n"),
    ok.


test_ets() ->
%%     Tid1 = ets:new(test_ets, [public]),
%%
%%     TableName = one_primary,
%%     R1 = {{one_primary, 1}, a},
%%     R2 = {{one_primary, 2}, b},
%%     R3 = {{two_primary, 3, 1}, c},
%%     R4 = {{two_primary, 3, 2}, d},
%%     [ets:insert(Tid1, R) || R <- [R1, R2, R3, R4]],
%%
%%     MS = ets:fun2ms(fun({{Table, PrimaryValue}, Value})
%%         when Table == TableName, PrimaryValue == 1->
%%         Value;
%%         ({{Table, PrimaryValue1, PrimaryValue2}, Value})
%%             when Table == two_primary, PrimaryValue1 == 3, PrimaryValue2 == 1->
%%             Value
%%     end),
%%     io:format("match result:~p~n", [ets:select(Tid1, MS)]),

    L = lists:merge(fun(A, B) -> A == B end, [1,2,3], [2,3,4]),
    io:format("merge:~p~n", [L]),
    ok.

%% 测试大规模数据批量保存需要花费的时间
test_save_cost_time() ->
    db_app:start(),
    {ok, Tid} = ets:file2tab("/home/jie/db1.ets"),
    io:format("start test_save_cost_time~n"),
    {Time, _} = timer:tc(db_timer_save, sync_save_to_db, [Tid]),
    io:format("end test_save_cost_time : ~p~n", [Time]).
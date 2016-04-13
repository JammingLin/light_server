%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 17:02
%%%-------------------------------------------------------------------
-module(id_generator).

-export([start_link/0, get_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).
-export([test/0]).

-record(state, {server_id}).

-define(COUNT_TO_SAVE, 100). %% 每间隔指定的数值后持久化到数据库中
-define(TABLE_NAME, id_generator). %% 对应的数据库中的表名

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获得一个未使用过的ID
get_id(Type) ->
    gen_server:call(?MODULE, {get_id, Type}).

init([]) ->
    ServerID = get_server_id(),
%%     application:set_env(dgry, server_id, ServerID),
    %% 只维护本服务器的ID
    Sql = lists:concat(["select * from ", ?TABLE_NAME, " where server_id=", ServerID]),
    Records = db:query(Sql),
    [begin
         Type = maps:get(type, Record),
         Count = maps:get(count, Record),
         ReviseCount = revise_count(Count),
         db:save(?TABLE_NAME, #{server_id=>ServerID, type=>Type, count=>ReviseCount}),
         Key = make_key(binary_to_list(Type)),
%%          cache:q(["set", Key, ReviseCount])
%%          cache:set(Key, ReviseCount)
         put(Key, ReviseCount)
     end || Record <- Records],
    {ok, #state{server_id = ServerID}}.

handle_call({get_id, Type}, _From, #state{server_id=ServerID}=State) ->
%%     {ok, ServerID} = application:get_env(dgry, server_id),
    Id = get_id(ServerID, Type),
    {reply, Id, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_id(ServerID, Type) when is_atom(Type) ->
    Key = make_key(Type),
%%     {ok, Cnt} = cache:q(["incr", Key]),
    Cnt = case get(Key) of
        undefined -> 0;
        C -> C
    end,
    Count = Cnt + 1,
    put(Key, Count),
    case Count of
        1 -> db:save(?TABLE_NAME, #{server_id=>ServerID, type=>Type, count=>Count});
        _ -> ok
    end,
    Count1 = case Count rem ?COUNT_TO_SAVE of
        0 ->
            Record = #{server_id=>ServerID, type => Type, count => Count},
            db:save(id_generator, Record),
            Count;
        _ ->
            Count
    end,
    ID = ServerID bsl (64-16) + Count1, %% ServerID占用最高位的16位, 其余位数给Count使用
    ID.

make_key(Type) ->
    list_to_binary(lists:concat([?TABLE_NAME, ":", Type])).

%% 修正数量值, 用于防止因为奔溃等原因导致的没有把当前数量值写入到数据库, 采用修正算法避免出现重复的情况
revise_count(CurrentCount) ->
    (CurrentCount div ?COUNT_TO_SAVE + 1) * ?COUNT_TO_SAVE.

get_server_id() ->
    IPList = get_ip_list(),
    Sql = "select * from server_id",
    Records = db:query(Sql),
    Pred = fun(#{ip:=IP}) ->
        lists:member(binary_to_list(IP), IPList)
    end,
    case lists:filter(Pred, Records) of
        [] ->
            IP1 = hd(IPList),
            Insert = lists:concat(["insert into server_id set ip = '", IP1, "'"]),
            {updated, Result} = db:execute2(Insert),
            ServerID = mysql:get_result_insert_id(Result),
            ServerID;
        [#{id:=ServerID1}] ->
            ServerID1
    end.

get_ip_list() ->
    {ok, IPList} = inet:getif(),
    [lists:concat([A1, ".", A2, ".", A3, ".", A4]) || {{A1, A2, A3, A4}=IP, _, _Mask} <- IPList, IP /= {127,0,0,1}].

%%--------------------------------test-----------------------------------------------
test() ->
    dgry_server:start('dgry_db@127.0.0.1'),
    test1(),
    test2(),
    io:format("test id_generator ok! ~n").

test1() ->
    Id = id_generator:get_id(test1),
    Id1 = id_generator:get_id(test1),
    Id1 = Id + 1.

test2() ->
    BaseID = id_generator:get_id(test2),
    Count = 100,
    SpawnCount = 10,
    SpwanFun = fun(Cnt) ->
        spawn(fun() -> [id_generator:get_id(test2) || _ <- lists:seq(1, Cnt)] end)
    end,
    [SpwanFun(Count) || _ <- lists:seq(1, SpawnCount)],
    timer:sleep(100),
    BaseID1 = id_generator:get_id(test2),
    BaseID1 = BaseID + Count * SpawnCount + 1.

%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 八月 2014 9:22
%%%-------------------------------------------------------------------
-module(robot).
-author("long").
-include("game_types.hrl").
-include("game_constants.hrl").
-define(SERVER_PORT, 5821).

-define(AES_KEY, <<144, 161, 21, 67, 57, 237, 71, 178, 229, 43, 14, 237, 177, 248, 142, 206, 12, 187, 23, 117, 100, 0, 229, 17, 189, 242, 173, 194, 54, 120, 30, 250>>).
-define(AES_IV, <<203, 228, 116, 249, 48, 124, 122, 12, 206, 137, 160, 132, 121, 2, 199, 140>>).

-define(DEBUG_TYPE, 1).

-compile(export_all).
-record(tclient, {service, protocol, seqid}).

run() ->
    robot:run("127.0.0.1", 1, 1, 100),
    timer:sleep(infinity).

run(ServerIP, Start, End, SpawnSpan) ->
    run(ServerIP, Start, End, SpawnSpan, one_way).

run(ServerIP, Start, End, SpawnSpan, Type) ->
    lager:start(),
    lager:set_loglevel(lager_console_backend,debug),
    m_rand:start_link(),
    custom_timer:start_link(End - Start + 1),
    case Type of
        one_way ->
            start_robot(ServerIP, true, Start, End, SpawnSpan, ?SERVER_PORT, ?DEBUG_TYPE);
        cluster  ->
            ok
    end,
    timer:sleep(infinity).


start_robot(ServerIP, IsAsync, Start, End, SpawnSpan, Port, DebugType)->
    lager:error("start robot"),
    gen_server:cast(custom_timer,{clear, End - Start + 1}),
    case IsAsync of
        true ->
            PidList = lists:foldl(fun(ID, L) ->
                Pid = proc_lib:spawn_link(fun() ->
                    Account = lists:concat(["test", ID]),
                    if
                        ID =:= End-> lager:error("All logind count:~p",[(End - Start + 1)]);
                        true -> ok
                    end,

                    robot_processer(ServerIP, Port, Account, "1", DebugType)

                end),
                NewL = lists:append(L, [Pid]),
                timer:sleep(trunc(1000/SpawnSpan-1)),
                NewL
            end, [], lists:seq(Start, End)),
            put(pid_list, PidList),
            PidList;
        false ->
            [begin
                 Account = lists:concat(["test", ID]),
                 robot_processer(ServerIP, Port, Account, "1", DebugType)
             end || ID <- lists:seq(Start, End)]
    end.

robot_processer(ServerIP, Port, Account, Password, DebugType) ->
    robot_core:start_link(),
    random_init(),
    %% 1.连接
    Client = connect(ServerIP, Port),
    rpc(1,Client, get_protocol_version, []),

    %% 2.登录
    PlayerId = login(Client, Account, Password),
    true = PlayerId > 1,
    put(playerid, PlayerId),
    case DebugType of
        1 ->
            login_success(Client, PlayerId, Account);
        0 ->
            login_success_simple(Client, PlayerId, Account)
    end,
    timer:sleep(infinity).

login(Client, Account, Password) ->
    okrpc(1,Client, login, [?game_Protocol_version, Account, Password]).

login_success_simple(Client, PlayerID, _Account) ->
    mark_time(),
    start_heartbeat_timer(),
    robot_loop(Client, PlayerID).

login_success(Client, PlayerID, _Account) ->
    lager:info("login success, playerid:~p", [PlayerID]),
    logined_scene(PlayerID, Client),
    mark_time(),
    start_heartbeat_timer(),
    {NewFun, NewArgs, _} = random_function(Client, PlayerID),
    erlang:send_after(10*1000, self(), {call_function, NewFun, NewArgs}),
    robot_loop(Client, PlayerID).

robot_loop(Client, PlayerID) ->
    receive
        {timeout, heartbeat} ->
            okrpc(PlayerID,Client, heartbeat, []),
            start_heartbeat_timer();
        {call_function, Function, Args} ->
            {_S1, S2, S3} = os:timestamp(),
            do_function(Function, Args),
            {_E1, E2, E3} = os:timestamp(),
            RpcTime = ((E2-S2) * 1000000 + (E3-S3))/1000,
            if
                RpcTime > 20000  ->
                    lager:error("PlayerID ~p call_function run ~p cost:~p(ms)", [PlayerID, Function, RpcTime]);
                true ->
                    ok
            end,
            {NewFun, NewArgs, Sleep} = random_function(Client, PlayerID),
            erlang:send_after(Sleep, self(), {call_function, NewFun, NewArgs})
    end,

    robot_loop(Client, PlayerID).

random_function(Client, PlayerID) ->
    SceneRates = [
        {40,{my_city_scene}},
        {0,{world_scene}}
    ],
    {NewScene} = utils:random_select(SceneRates),
    {NewFun, NewArgs, Sleep1, Sleep2} = apply(?MODULE, NewScene, [Client,PlayerID]),
    Sleep = m_rand:random(Sleep1, Sleep2),
    {NewFun, NewArgs, Sleep}.

do_function(Function, Args)->
    apply(?MODULE, Function, Args).

get_server_time(Client, PlayerID)->
    rpc(PlayerID, Client, get_server_time, []).

start_heartbeat_timer() ->
    robot_core:start_timer(self(), heartbeat, 10 * 1000),
    ok.

mark_time() ->
    case custom_timer:mark() of
        ok ->
            ok;
        Time ->
            lager:error("time: ~p", [Time])
    end.

%% -- 登录后场景
logined_scene(PlayerID, Client) ->
    rpc(PlayerID, Client, get_server_time, []).

logout(Client) ->
    PlayerId = get(playerid),
    rpc(PlayerId,Client, logout, []).

%% 主城场景
my_city_scene(Client, PlayerID)->
    FunRates = [
        {10,  {get_server_time, [Client, PlayerID], 60000, 120000}}
    ],

    Function=utils:random_select(FunRates),
    {F,_,_,_} = Function,
    lager:debug("my_city_scene ~p ~p", [F, PlayerID]),
    Function.


%% ************* 基础函数

connect(IP, Port) ->
    {NormalClient, SecureClient} = new_client(IP, Port, [{transport, thrift_secure_transport}]),
    proto_handshake(NormalClient, SecureClient).

connect(IP, Port, PubKey) ->
    {NormalClient, SecureClient} = new_client(IP, Port, [{transport, thrift_secure_transport}]),
    proto_handshake(NormalClient, SecureClient, PubKey).

proto_handshake(NormalClient, SecureClient) ->
    Pubkey = read_rsa_key("data/keys/rsa_pub.key"),
    proto_handshake(NormalClient, SecureClient, Pubkey).

read_rsa_key(FileName) ->
    {ok, PemBin} = file:read_file(FileName),
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

proto_handshake(NormalClient = #tclient{protocol = NormalProto}, #tclient{protocol = SecureProto}, Pubkey) ->
    %% 模拟客户端读取加密过的公钥， 这边只要简单的从文件读取就好
    {Proto1, {ok, _PubKey0}} = thrift_protocol:read(NormalProto, string),

    %% 生成aes的key给服务端

    put(aes_key, ?AES_KEY),
    put(aes_vector, ?AES_IV),
    SecureKey = rsa:encrypt(?AES_KEY, Pubkey),
    SecureIV = rsa:encrypt(?AES_IV, Pubkey),
    {Proto2, ok} = thrift_protocol:write(Proto1, {string, SecureKey}),
    {Proto3, ok} = thrift_protocol:write(Proto2, {string, SecureIV}),
    {_Proto4, ok} = thrift_protocol:flush_transport(Proto3),
    NormalClient#tclient{protocol = SecureProto}.

close(Client) ->
    logout(Client),
    thrift_client:close(Client),
    ok.

new_client(Host, Port, Options) when is_integer(Port), is_list(Options) ->
    Service = game_thrift,
    SockOpts = [binary,
        {packet, 0},
        {active, false},
        {nodelay, true},{reuseaddr,true}],
    case catch gen_tcp:connect(Host, Port, SockOpts, infinity) of
        {ok, Sock} ->
            {ok, Transport}       = thrift_socket_transport:new(Sock, [{recv_timeout, 5000}]),
            {ok, FrameTransPort}  = thrift_framed_transport:new(Transport),
            {ok, NormalProto}     = thrift_binary_protocol:new(FrameTransPort, [{strict_read, true}, {strict_write, true}]),
            {ok, NormalClient}    = thrift_client:new(NormalProto, Service),

            {ok, SecureTransport} = thrift_secure_transport:new(Transport, ?AES_KEY, ?AES_IV),
            {ok, SecureProto}     = thrift_binary_protocol:new(SecureTransport, [{strict_read, true}, {strict_write, true}]),
            {ok, SecureClient}    = thrift_client:new(SecureProto, Service),
            {NormalClient, SecureClient};
        Error ->
            Error
    end.

rpc(_PlayerId, Client, Function, Args) ->
%%     start_rpc_call(),
    ws_rpc(Client, Function, Args).

okrpc(PlayerId, Client, Function, Args) ->
    case rpc(PlayerId, Client, Function, Args) of
        {ok,{_Client, {ok, Result}}}->Result;
        {exception,{Reason, ExceptionType}} -> lager:debug("Exception ~p ~p", [Reason, ExceptionType]);
        {ok, Result}-> Result;
        Reason -> lager:debug("okprc Reason ~p ", [Reason])
    end.

loop(_Sleep, Max, Max, F) ->
    F(Max);

loop(Sleep, N, Max, F) ->
    F(N),
    timer:sleep(Sleep),
    loop(Sleep, N + 1, Max, F).

random_sleep(Sec1, Sec2) ->
    Sleep = m_rand:random(Sec1, Sec2),
    timer:sleep(Sleep).

random_init() ->
    {A1, A2, A3} = erlang:timestamp(),
    m_rand:seed(A1, A2, A3).

start_rpc_call()->
    gen_server:cast(robot_sum, rpc_call).

rpc_time()->
    {RpcAllTimes, CostTime} = gen_server:call(robot_sum, rpc_call_stop),
    lager:error("stop_rpc_call result AllTimes : ~p, CostTime:~p, Avg:~p",
        [RpcAllTimes, CostTime, CostTime/RpcAllTimes]).

ws_rpc(Client, Function, Args) ->
    Client2 = case get(client) of
                  undefined -> Client;
                  Client1 -> Client1
              end,

    Client3 = Client2#tclient{seqid = Client2#tclient.seqid + 1},
    put(client,Client3),
    case thrift_client:call(Client3, Function, Args) of
        {Client4, {ok, _} = Ret} = OK ->
            io:format("ok ret: ~p~n", [Ret]),
            put(client, Client4),
            {ok, OK};
        {Client4, {error, closed}} ->
            put(client, Client4),
            exit(shutdown);
        {Client4,{error,{bad_seq_id,_}}} ->
            put(client, Client4),
            {ok,ok};
        {Client4, {error, Reason}} ->
            io:format("rpc call error Reason:~p, Fun:~p Args:~p!!!!!~n",
                [Reason, Function, Args]),
            put(client, Client4),
            {error,Reason};
        {Client4, {exception, Exception}} ->
            put(client, Client4),
            io:format("rpc call exception:~p~n", [Exception]),
            {exception, Exception};
        {Client4, Rslt} = Other ->
            io:format("ok ret: ~p~n", [Rslt]),
            put(client, Client4),
            {ok,Other};
        Error ->
            io:format("error ~p~n", [Error]),
            Error
    end.

start_link() ->
    Client = connect("127.0.0.1", 5821),
    rpc(0, Client, get_message, [<<"hello">>]).
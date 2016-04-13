%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2015 下午2:28
%%%-------------------------------------------------------------------
-module(gate_server).
-behaviour(gen_server).
%% -behaviour(ranch_protocol).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").


%% API.
-export([child_spec/0, start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(MAX_CONNS, 100000). %% 最大连接数
-define(GC_TIMEOUT, 5000).
-record(thrift_processor, {transport, handler, protocol, service, handshake}).

child_spec() ->
    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
        permanent, 5000, supervisor, [ranch_sup]},
    Service = game_thrift,
    Handler = gate_player,
    ListenerSpec = ranch:child_spec(gate_server, 50,
        ranch_tcp, [{port, get_port()}, {max_connections, ?MAX_CONNS}],
        ?MODULE, [Service, Handler]),
    {RanchSupSpec, ListenerSpec}.

get_port() ->
    case init:get_argument(port) of
        error -> 5821;
        {ok, [[Port]]} -> list_to_integer(Port)
    end.


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, [Service, Handler]) ->
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{packet, 4}, {active, once}]),

    HandshakeProto = send_handshake_pubkey(Socket),
    gen_server:enter_loop(?MODULE, [],
        #thrift_processor{protocol = HandshakeProto,
        service = Service,
        handler = Handler,
        transport = Transport,
        handshake = false}).

send_handshake_pubkey(Socket) ->
%%     try
        PubKey = rsa:get_public_key(),

        {ok, SocketTransport1}  = thrift_socket_transport:new(Socket),
        {ok, FrameTransport}    = my_framed_transport:new(SocketTransport1),
        {ok, HandshakeProto}    = thrift_binary_protocol:new(FrameTransport),
        {Proto1, ok} = thrift_protocol:write(HandshakeProto, {string, PubKey}),
        {Proto2, ok} = thrift_protocol:flush_transport(Proto1),
        Proto2.
%%     catch Type:Data ->
%%         error_logger:error_msg("create_secure_channel exception, Type:~p, Data:~p~n", [Type, Data]),
%%         exit(shutdown)
%%     end.

handle_info({tcp, Socket, Bin}, State=#thrift_processor{protocol = Proto,
    transport=Transport, handshake=false, handler=Handler}) ->
    put(tcp_data, Bin),

    {Proto3, {ok, SecureKey}} = thrift_protocol:read(Proto, string),
    Key = rsa:decrypt(SecureKey),

    {_Proto4, {ok, SecureVector}} = thrift_protocol:read(Proto3, string),
    Vector = rsa:decrypt(SecureVector),
    SecureProto = secure_proto(Socket, Key, Vector),

    Handler:init(SecureProto),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State#thrift_processor{handshake=true, protocol = SecureProto}, ?GC_TIMEOUT};
handle_info({tcp, Socket, Bin}, State=#thrift_processor{transport=Transport}) ->
    put(tcp_data, Bin),
    {State1, _ } = proc_msg(Socket, State),

    Transport:setopts(Socket, [{active, once}]),
    {noreply, State1, ?GC_TIMEOUT};
handle_info({tcp_closed, _Socket}, #thrift_processor{handler = Handler} = State) ->
    Handler:handle_error(undefined, closed),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, #thrift_processor{handler = Handler} = State) ->
    Handler:handle_error(undefined, {tcp_error, Reason}),
    {stop, Reason, State};
handle_info({'EXIT', FromPid, Reason}, #thrift_processor{handler = Handler} = State) ->
    Handler:handle_error(exit, {FromPid, Reason}),
    {stop, normal, State};
handle_info(server_is_full,  #thrift_processor{handler = Handler} = State) ->
    Handler:handle_reduse_connect(server_is_full),
    {noreply, State, ?GC_TIMEOUT};
handle_info(heartbeat_timeout,  #thrift_processor{handler = Handler} = State) ->
    Handler:handle_heartbeat_timeout(),
    {stop, normal, State};
handle_info(timeout, State) ->
    {noreply, State, hibernate};
handle_info(_Info, State) ->
    {noreply, State, ?GC_TIMEOUT}.

handle_call({kickout, Reason}, FromPid, #thrift_processor{handler = Handler} = State) ->
    Handler:handle_error(kickout, {FromPid, Reason}),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?GC_TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State, ?GC_TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

secure_proto(Socket, AesKey, AesVector) ->
    {ok, SocketTransport}   = thrift_socket_transport:new(Socket),
    {ok, Transport1}        = my_secure_transport:new(SocketTransport, AesKey, AesVector),
    {ok, SecureProto}       = thrift_binary_protocol:new(Transport1),
    SecureProto.

proc_msg(_Socket, State0 = #thrift_processor{protocol = Proto0, handler = Handler}) ->
    {Proto1, MessageBegin} = thrift_protocol:read(Proto0, message_begin),
    State1 = State0#thrift_processor{protocol = Proto1},
    case MessageBegin of
        #protocol_message_begin{name = Function,
            type = ?tMessageType_CALL,
            seqid = Seqid} ->
            case handle_function(State1, list_to_atom(Function), Seqid) of
                {_State2, ok} = OK -> OK;
                {_State2, {error, _Reason}} = Error ->
                    Handler:handle_error(list_to_atom(Function), _Reason),
                    thrift_protocol:close_transport(Proto1),
                    Error
            end;
        #protocol_message_begin{name = Function,
            type = ?tMessageType_ONEWAY,
            seqid = Seqid} ->
            case handle_function(State1, list_to_atom(Function), Seqid) of
                {_State2, ok} = OK ->
                    OK;
                {_State2, shutdown} ->
                    Handler:handle_error(list_to_atom(Function), shutdown),
                    thrift_protocol:close_transport(Proto1),
                    exit(shutdown);
                {_State2, {error, _Reason}} = Error->
                    Handler:handle_error(list_to_atom(Function), _Reason),
                    thrift_protocol:close_transport(Proto1),
                    Error
            end;
        {error, timeout = _Reason} ->
            Handler:handle_error(undefined, timeout),
            ok;
        {error, closed = Reason} ->
            Handler:handle_error(undefined, Reason),
            thrift_protocol:close_transport(Proto1),
            exit(shutdown);
        {error, Reason} ->
            lager:debug("error other Reason ~p", [Reason]),
            Handler:handle_error(undefined, Reason),
            thrift_protocol:close_transport(Proto1),
            exit(shutdown)
    end.

handle_function(State0 = #thrift_processor{protocol = Proto0,
    handler = Handler,
    service = Service},
    Function,
    Seqid) ->
    case check_seqid(Seqid) of
        false ->
            error_logger:error_msg("seqid error, Seqid:~p, old_seqid:~p", [Seqid, get(network_seqid)]),
            {State0, {error, seqid_error}};
        true ->
            try
                InParams = Service:function_info(Function, params_type),
                {Proto1, {ok, Params}} = thrift_protocol:read(Proto0, InParams),
                State1 = State0#thrift_processor{protocol = Proto1},

%%                 record_network:record({Function, Params}),
%%                 put(thrift_proto, Proto1),
                Result = Handler:handle_function(Function, Params),
                %% {Micro, Result} = better_timer(Handler, handle_function, [Function, Params]),
                handle_success(State1, Function, Result, Seqid)
            catch
                throw:Data ->
                    InParams1 = Service:function_info(Function, params_type),
                    {Proto11, {ok, _}} = thrift_protocol:read(Proto0, InParams1),
                    State11 = State0#thrift_processor{protocol = Proto11},
                    handle_function_catch(State11, Function, throw, Data, Seqid);
                error:Data ->
                    handle_function_catch(State0, Function, error, Data, Seqid)
            end
    end.

check_seqid(Seqid) ->
    Seqid2 = case get(network_seqid) of
                 undefined ->
                     put(network_seqid, Seqid),
                     0;
                 Seqid1 -> Seqid1
             end,
    Seqid > Seqid2.

handle_function_catch(State = #thrift_processor{service = Service},
    Function, ErrType, ErrData, Seqid) ->
    IsOneway = Service:function_info(Function, reply_type) =:= oneway_void,

    case {ErrType, ErrData} of
        _ when IsOneway ->
            Stack = erlang:get_stacktrace(),
            error_logger:warning_msg(
                "oneway void ~p threw error which must be ignored: ~p",
                [Function, {ErrType, ErrData, Stack}]),
            {State, ok};

        {throw, Exception} when is_tuple(Exception), size(Exception) > 0 ->
            error_logger:warning_msg("~p threw exception: ~p~n", [Function, Exception]),
            handle_exception(State, Function, Exception, Seqid),
            {State, ok};
        % we still want to accept more requests from this client

        {error, Error} ->
            handle_error(State, Function, Error, Seqid),
            {State, {error, Error}}
    end.

handle_success(State = #thrift_processor{service = Service},
    Function,
    Result,
    Seqid) ->
    ReplyType = Service:function_info(Function, reply_type),
    StructName = atom_to_list(Function) ++ "_result",
    case Result of
        {reply, ReplyData} ->
            Reply = {{struct, [{0, ReplyType}]}, {StructName, ReplyData}},
            send_reply(State, Function, ?tMessageType_REPLY, Reply, Seqid);

        {shutdown, ReplyData} ->
            Reply = {{struct, [{0, ReplyType}]}, {StructName, ReplyData}},
            {Status, _} = send_reply(State, Function, ?tMessageType_REPLY, Reply, Seqid),
            {Status, shutdown};

        ok when ReplyType == {struct, []} ->
            send_reply(State, Function, ?tMessageType_REPLY, {ReplyType, {StructName}}, Seqid);

        ok when ReplyType == oneway_void ->
            %% no reply for oneway void
            {State, ok}
    end.



handle_exception(State = #thrift_processor{service = Service},
    Function,
    Exception,
    Seqid) ->
    ExceptionType = element(1, Exception),
    %% Fetch a structure like {struct, [{-2, {struct, {Module, Type}}},
    %%                                  {-3, {struct, {Module, Type}}}]}

    ReplySpec = Service:function_info(Function, exceptions),
    {struct, XInfo} = ReplySpec,

    true = is_list(XInfo),

    %% Assuming we had a type1 exception, we'd get: [undefined, Exception, undefined]
    %% e.g.: [{-1, type0}, {-2, type1}, {-3, type2}]
    ExceptionList = [case Type of
                         ExceptionType -> Exception;
                         _ -> undefined
                     end
        || {_Fid, {struct, {_Module, Type}}} <- XInfo],

    ExceptionTuple = list_to_tuple([Function | ExceptionList]),

    % Make sure we got at least one defined
    case lists:all(fun(X) -> X =:= undefined end, ExceptionList) of
        true ->
            handle_unknown_exception(State, Function, Exception, Seqid);
        false ->
            send_reply(State, Function, ?tMessageType_REPLY, {ReplySpec, ExceptionTuple}, Seqid)
    end.

%%
%% Called when an exception has been explicitly thrown by the service, but it was
%% not one of the exceptions that was defined for the function.
%%
handle_unknown_exception(State, Function, Exception, Seqid) ->
    handle_error(State, Function, {exception_not_declared_as_thrown,
        Exception}, Seqid).

handle_error(State, Function, Error, Seqid) ->
    Stack = erlang:get_stacktrace(),
    error_logger:error_msg("~p had an error: ~p~n", [Function, {Error, Stack}]),

    %% todo:开发期间先全部推送到客户端； thrift不是以application启动，度不到env
    Message = lists:flatten(io_lib:format("An error occurred: ~p~n",
        [{Error, Stack}])),

%%     Message =
%%         case application:get_env(thrift, exceptions_include_traces) of
%%             {ok, true} ->
%%                 lists:flatten(io_lib:format("An error occurred: ~p~n",
%%                     [{Error, Stack}]));
%%             _ ->
%%                 "An unknown handler error occurred."
%%         end,
    Reply = {?TApplicationException_Structure,
        #'TApplicationException'{
            message = Message,
            type = ?TApplicationException_UNKNOWN}},
    send_reply(State, Function, ?tMessageType_EXCEPTION, Reply, Seqid).

send_reply(State = #thrift_processor{protocol = Proto0}, Function, ReplyMessageType, Reply, Seqid) ->
    try
        {Proto1, ok} = thrift_protocol:write(Proto0, #protocol_message_begin{
            name = atom_to_list(Function),
            type = ReplyMessageType,
            seqid = Seqid}),
        {Proto2, ok} = thrift_protocol:write(Proto1, Reply),
        {Proto3, ok} = thrift_protocol:write(Proto2, message_end),
        {Proto4, ok} = thrift_protocol:flush_transport(Proto3),
        {State#thrift_processor{protocol = Proto4}, ok}
    catch
        error:{badmatch, {_, {error, _} = Error}} ->
            {State, Error}
    end.

%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(thrift_processor).

-export([init/1, loop/1]).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-record(thrift_processor, {handler, protocol, service}).

init({_Server, HandshakeProtoGen, SecureProtoGen, Service, Handler}) ->
    process_flag(trap_exit, true),
    HandshakeProto = HandshakeProtoGen(),
    SecureProto = create_secure_proto(HandshakeProto, SecureProtoGen),
    loop(#thrift_processor{protocol = SecureProto,
        service = Service,
        handler = Handler}).

create_secure_proto(HandshakeProto, SecureProtoGen) ->
    try
        PubKey = rsa:get_public_key(),

        {Proto1, ok} = thrift_protocol:write(HandshakeProto, {string, PubKey}),
        {Proto2, ok} = thrift_protocol:flush_transport(Proto1),

        {Proto3, {ok, SecureKey}} = thrift_protocol:read(Proto2, string),
        Key = rsa:decrypt(SecureKey),
%%         put(aes_key, Key),

        {_Proto4, {ok, SecureVector}} = thrift_protocol:read(Proto3, string),
        Vector = rsa:decrypt(SecureVector),
%%         put(aes_vector, Vector),
        SecureProtoGen(Key, Vector)
    catch Type:Data ->
        error_logger:error_msg("create_secure_channel exception, Type:~p, Data:~p~n", [Type, Data]),
        exit(shutdown)
    end.

loop(State0 = #thrift_processor{protocol = Proto0, handler = Handler}) ->
    receive
        {'EXIT', FromPid, EReason} ->
            Handler:handle_error(exit, {FromPid, EReason}),
            thrift_protocol:close_transport(Proto0),
            exit(shutdown);
        Msg -> lager:info("thrift processor recv msg: ~p",[Msg]),
            ok
    after 0 -> ok
    end,
    {Proto1, MessageBegin} = thrift_protocol:read(Proto0, message_begin),
    State1 = State0#thrift_processor{protocol = Proto1},
    case MessageBegin of
        #protocol_message_begin{name = Function,
            type = ?tMessageType_CALL,
            seqid = Seqid} ->
            case handle_function(State1, list_to_atom(Function), Seqid) of
                {State2, ok} -> loop(State2);
                {_State2, {error, Reason}} ->
                    Handler:handle_error(list_to_atom(Function), Reason),
                    thrift_protocol:close_transport(Proto1),
                    ok;
                {_State2, {exit, Reason}} ->
                    Handler:handle_error(list_to_atom(Function), Reason),
                    thrift_protocol:close_transport(Proto1),
                    exit(shutdown)
            end;
        #protocol_message_begin{name = Function,
            type = ?tMessageType_ONEWAY,
            seqid = Seqid} ->
            case handle_function(State1, list_to_atom(Function), Seqid) of
                {State2, ok} -> loop(State2);
                {_State2, {error, Reason}} ->
                    Handler:handle_error(list_to_atom(Function), Reason),
                    thrift_protocol:close_transport(Proto1),
                    ok;
                {_State2, {exit, Reason}} ->
                    Handler:handle_error(list_to_atom(Function), Reason),
                    thrift_protocol:close_transport(Proto1),
                    exit(shutdown)
            end;
        {error, timeout = _Reason} ->
            Handler:handle_error(undefined, timeout),
            loop(State1);
        {error, closed = Reason} ->
%%          error_logger:info_msg("Client disconnected~n"),
%%          record_network:record({error, Reason}),
            Handler:handle_error(undefined, Reason),
            thrift_protocol:close_transport(Proto1),
            exit(shutdown);
        {error, Reason} ->
%%          record_network:record({error, Reason}),
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
            InParams = Service:function_info(Function, params_type),

            {Proto1, {ok, Params}} = thrift_protocol:read(Proto0, InParams),
            State1 = State0#thrift_processor{protocol = Proto1},

            try
                put(thrift_proto, Proto1),
                Result = Handler:handle_function(Function, Params),
                handle_success(State1, Function, Result, Seqid)
            catch
                Type:Data when Type =:= throw orelse Type =:= error ->
                    handle_function_catch(State1, Function, Type, Data, Seqid)
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
            %error_logger:warning_msg("~p threw exception: ~p~n", [Function, Exception]),
            handle_exception(State, Function, Exception, Seqid);
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

        ok when ReplyType == {struct, []} ->
            send_reply(State, Function, ?tMessageType_REPLY, {ReplyType, {StructName}}, Seqid);

        ok when ReplyType == oneway_void ->
            %% no reply for oneway void
            {State, ok};

        {crypto_proto, true, Key, IV} ->
            Ret = send_reply(State, Function, ?tMessageType_REPLY, {ReplyType, {StructName}}, Seqid),
            set_crypto_transport(Key, IV),
            Ret
    end.

set_crypto_transport(_Key, _IV) ->
%% 	put(proto_crypto, true),
%% 	put(crypto_Key, Key),
%% 	put(proto_iv, IV).
    ok.


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

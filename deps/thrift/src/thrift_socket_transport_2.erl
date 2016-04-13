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

-module(thrift_socket_transport_2).

-behaviour(thrift_transport).

-export([new/1,
    new/2,
    write/2, read/2, flush/1, close/1,

    new_transport_factory/3]).

-record(data, {socket,
    recv2_timeout=infinity}).
-type state() :: #data{}.
-include("thrift_transport_behaviour.hrl").

new(Socket) ->
    new(Socket, []).

new(Socket, Opts) when is_list(Opts) ->
    State =
        case lists:keysearch(recv2_timeout, 1, Opts) of
            {value, {recv2_timeout, Timeout}}
                when is_integer(Timeout), Timeout > 0 ->
                #data{socket=Socket, recv2_timeout=Timeout};
            _ ->
                #data{socket=Socket}
        end,
    thrift_transport:new(?MODULE, State).

%% Data :: iolist()
write(This, Data) ->
    thrift_socket_transport:write(This, Data).

read(This = #data{socket=Socket, recv2_timeout=Timeout}, Len)
    when is_integer(Len), Len >= 0 ->
    case gen_tcp:recv(Socket, Len, Timeout) of
        Err = {error, timeout} ->
            {This, Err};
        Data ->
            {This, Data}
    end.

%% We can't really flush - everything is flushed when we write
flush(This) ->
    thrift_socket_transport:flush(This).

close(This) ->
    thrift_socket_transport:close(This).


%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generates a "transport factory" function - a fun which returns a thrift_transport()
%% instance.
%% This can be passed into a protocol factory to generate a connection to a
%% thrift server over a socket.
%%
new_transport_factory(Host, Port, Options) ->
    thrift_socket_transport:new_transport_factory(Host, Port, Options).

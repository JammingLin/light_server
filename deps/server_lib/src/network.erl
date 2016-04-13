%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 七月 2014 17:02
%%%-------------------------------------------------------------------
-module(network).
-author("Administrator").

%% API
-export([send/3, broadcast/2, broadcast/3, broadcast/4, is_online/1]).
-export([get_ipaddress/1]).

send(PlayerID, NotificationName, Data) when is_integer(PlayerID)->
    gate_net:send(PlayerID, NotificationName, Data).

broadcast(TargetList, Data) ->
    gate_net:broadcast(TargetList, Data).

broadcast(PlayerId, NotificationName, Data) ->
    gate_net:broadcast(PlayerId, NotificationName, Data).

broadcast(PlayerId, TargetList, NotificationName, Data) ->
    gate_net:broadcast(PlayerId, TargetList, NotificationName, Data).

get_ipaddress(Proto) ->
    {_p,_tbp,{_bp,{_t,_tft,{_ft,{_t,_tst,{data,Socket,_i}},_,_,_,_}},_,_}} = Proto,
    {ok, {Address, Port}} = inet:peername(Socket),
    {inet:ntoa(Address), Port}.

is_online(PlayerID) ->
    rpc:call(monitor_nodes:get_gate_node(), gate_net, is_online, [PlayerID], 30000).
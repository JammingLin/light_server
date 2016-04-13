-module(stop_server_msg).

-export([stop/1]).

-include("../include/game_types.hrl").
stop([Node, Cookie, Time]) ->
    erlang:set_cookie(node(), Cookie),
    pong = net_adm:ping(Node),
    Time1 = list_to_integer(atom_to_list(Time)),
    rpc:call(Node, gate_net, send_all, [stop_server_msg, #stop_server_msg{rest_time = Time1}]),
    io:format("send all player stop server message!~n"),
    ok.


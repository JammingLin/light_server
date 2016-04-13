%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 八月 2014 14:25
%%%-------------------------------------------------------------------
-module(record_play).
-author("long").

%% API
-export([play/0]).

play() ->
    %% TODO: 初始化随机数种子
    ets:new(mapping_record_pid, [public,set,named_table]),
    Filenames = record_file:get_network_files(),
    [spawn(fun()-> init(Filename) end) || Filename <- Filenames].

init(Filename) ->
    Terms = record_file:get_network_content(Filename),
    [{start_timestamp, StartTimeStamp} | Rest] = Terms,
    loop(StartTimeStamp, 0, Rest).

loop(_StartTimeStamp, _LastDiffTime, []) ->
    ok;
loop(StartTimeStamp, LastDiffTime, [{DiffTime, Value} | Rest]) ->
    Sleep = DiffTime - LastDiffTime,
    timer:sleep(Sleep),
    %% TODO: 改成和rpc_handler中配置的handler和Service一样
    Handler = rpc_handler,
    Service = knowledge_thrift,
    case Value of
        network_accepted -> ok;
        _ -> mock_network_processor:handle(Service, Handler, Value)
    end,
    loop(StartTimeStamp, DiffTime, Rest).
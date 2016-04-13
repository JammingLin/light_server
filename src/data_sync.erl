%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 九月 2014 14:09
%%%-------------------------------------------------------------------
-module(data_sync).
-author("Administrator").


%% 数据同步加锁, 避免并发修改下数据不一致

%% API
-export([transaction/3, transaction/4]).


transaction(TableName, PrimaryValue, Fun) ->
    Id = {{TableName, PrimaryValue}, self()},
    global:trans(Id, Fun).

transaction(TableName, PrimaryValue1, PrimaryValue2, Fun) ->
    Id = {TableName, PrimaryValue1, PrimaryValue2},
    global:trans(Id, Fun).

-compile(export_all).
test_sync() ->
    ets:new(mysql_schema, [public,set,named_table]),
    TableInfo = #{
        columns =>
            [{player_id,bigint},
            {gold,int}],
        primary_name_list => [player_id]},
    Table = player,
    ets:insert(mysql_schema, {Table, TableInfo}),

    {ok, _} = cache_redis:start_link(),
    cache:delete_all(),

    L = lists:seq(1, 300),
    [spawn(fun()->
        PlayerID = ID,
        transaction(Table, PlayerID, fun() ->
            case cache:get(Table, PlayerID) of
                [] ->
                    Player1 = #{player_id=>PlayerID, gold => 3},
                    cache:save(Table, Player1);
                Player ->
                    Player1 = Player#{gold := 3 + maps:get(gold, Player)},
                    cache:save(Table, Player1)
            end
        end)
    end) || ID <- L],
    timer:sleep(1 * 100),

    Player = cache:get(player, 1),
    io:format("Player:~p~n", [Player]),
%%     {ok, Keys} = cache:q(["keys", "*"]),
%%     io:format("keys length:~p~n", [length(Keys)]),
%%     timer:sleep(infinity),
    ok.




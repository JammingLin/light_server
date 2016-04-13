%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jun 2015 上午9:55
%%%-------------------------------------------------------------------
-module(db_mysql_cluster_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 60}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
        fun ({module_info,_}) -> false;
            ({all,_}) -> false;
            ({init_per_suite,1}) -> false;
            ({end_per_suite,1}) -> false;
            ({generate_test_data,1}) -> false;
            ({_,1}) -> true;
            ({_,_}) -> false
        end, Functions)].

init_per_suite(Config) ->
    application:set_env(db, db_module, db_mysql_cluster),
    ok = application:start(db),
    delete_table(),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(db),
    ok.

delete_table() ->
    db:execute2("drop table if exists one_primary"),
    db:execute2("drop table if exists two_primary").


test_get(_) ->
    db_mysql_common:execute2(mysql, "delete from passport"),
    db_mysql_common:execute2(mysql1, "delete from passport"),

    [] = db:get(passport, "1"),

    R = #{account=> <<"1">>, password=> <<"1">>, player_id=>1},
    db:save(passport, R),
    [R] = db:get(passport, <<"1">>),
    ok.

test_move_data(_Config) ->
    {ok, Pool1} = application:get_env(db, db_pools),
    Count = 100,
    generate_test_data(Count),

    Pool2 = [{mysql, [
        {size, 20},
        {host, "127.0.0.1"},
        {user, "root"},
        {password, ""},
        {database, "dgry_test_1"},
        {port, 3306},
        {encoding, utf8}]}
    ],
    lists:foreach(fun(Pool) ->
        db_mysql_cluster:move(Pool),

        %% 下面是验证代码
        lists:foreach(fun(ID) ->
            Account = integer_to_binary(ID),
            case db_helper:get(passport, Account) of
                [#{account := Account}] -> ok;
                Ret -> throw({error, Pool, Account, Ret})
            end
        end, lists:seq(1, Count)),

        lists:foreach(fun(ID) ->
            lists:foreach(fun(HoleID) ->
                case db_helper:get(attack_camp, ID, HoleID) of
                    [#{player_id := ID, hole_id := HoleID}] -> ok;
                    Ret -> throw({error, Pool, ID, HoleID, Ret})
                end
            end, lists:seq(1, 20))
        end, lists:seq(1, Count))
    end, [Pool1, Pool2]).


generate_test_data(Count) ->
    clear_data(),
    [begin
         Query = lists:concat(["insert into passport set account = '",ID,"', password = '1', player_id = 0"]),
         db_mysql_common:execute(mysql, Query)
     end || ID <- lists:seq(1, Count)],

    [begin
         [begin
              Query = lists:concat(["insert into attack_camp set ",
                  "player_id = ", ID,
                  ", hole_id = ", HoldID,
                  ", army_id = 0, camp_status=0, finish_time = '2015-02-25 00:00:00', last_army_type = 0"]),
              db_mysql_common:execute(mysql, Query)
          end || HoldID<- lists:seq(1, 20)]
     end || ID <- lists:seq(1, Count)].

clear_data() ->
    db_mysql_common:execute2(mysql, "delete from cluster_nodes"),

    db_mysql_common:execute2(mysql, "delete from passport"),
    db_mysql_common:execute2(mysql1, "delete from passport"),
    db_mysql_common:execute2(mysql, "delete from attack_camp"),
    db_mysql_common:execute2(mysql1, "delete from attack_camp").

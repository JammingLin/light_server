%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jun 2015 上午11:48
%%%-------------------------------------------------------------------
-module(db_app_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
        fun ({module_info,_}) -> false;
            ({all,_}) -> false;
            ({init_per_suite,1}) -> false;
            ({end_per_suite,1}) -> false;
            ({_,1}) -> true;
            ({_,_}) -> false
        end, Functions)].

test_start(_) ->
    application:set_env(db, sqldir, "../../../data/sql/"),
    ok = application:start(db),
    ok = application:stop(db).
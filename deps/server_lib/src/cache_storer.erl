%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 三月 2015 17:25
%%%-------------------------------------------------------------------
-module(cache_storer).
-author("Administrator").

-define(KEYS_FILE_PATH, "data/cache_keys.dat").
-define(CACHE_MODULE, cache_redis). %% TODO: 实际上需要与 db_cache 里的配置一致

%% API
-export([store/0, restore/0]).
-export([test/0]).

store() ->
    Keys = ?CACHE_MODULE:get_db_keys(),
    store_keys(Keys).

store_keys(Keys) when is_list(Keys) ->
    FileName = ?KEYS_FILE_PATH,
    {ok, S} = file:open(FileName, [write]),
    io:format(S, "~p.", [Keys]),
    ok.

restore() ->
    Keys = load_keys(),
    restore_cache(Keys).

restore_cache(Keys) ->
    [load_data(Key) || Key <- Keys].

load_keys() ->
    FileName = ?KEYS_FILE_PATH,
    case file:consult(FileName) of
        {ok, [Temp]} ->
            Temp;
        _ ->
            []
    end.

load_data({Table, PrimaryKey}) ->
    case is_db_table(Table) of
        true ->
            db_cache:get(Table, PrimaryKey);
        false ->
            ignore
    end;
load_data({Table, PrimaryKey1, PrimaryKey2}) ->
    case is_db_table(Table) of
        true ->
            %%todo:需要优化，减少数据库读取次数
            db_cache:get(Table, PrimaryKey1, PrimaryKey2);
        false ->
            ignore
    end;
load_data(_Key) ->
    ignore.

is_db_table(_Table) ->
    %% TODO:从db_schema 判断
    true.

test() ->
    Keys = [{"editor_object_list","281474976710848"},
        {"player","281474976710796"},
        {"editor_object","281474976710873","1"},
        {"editor_object","281474976710796","4"},
        {"editor_object","281474976710843","4"}],
    store_keys(Keys),

    Keys1 = load_keys(),
    true = Keys == Keys1.
%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 八月 2014 14:07
%%%-------------------------------------------------------------------
-module(utils).
-author("long").

%% API
-export([shuffle/1, eval/1, term_to_string/1, random_select/1, random_one/1, pick_one/1, limit/3, floor/1, ceiling/1]).
-export([data_to_binary/1, binary_to_data/1, data_to_string/1, string_to_data/1, utf8_string_length/1]).
-export([split/2]).
-export([reload_config/2, is_illegal_string/1]).

%% 洗牌, 对列表进行打乱随机排序
shuffle(List) ->
    shuffle(List, []).

shuffle([], Acc) ->
    Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).

eval(String) when is_list(String) ->
    {ok, Scanned, _} = erl_scan:string(String),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value, _} = erl_eval:exprs(Parsed, []),
    Value.

term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

data_to_binary(Data) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Data]))).

binary_to_data(Binary) ->
    eval(binary_to_list(Binary) ++ ".").

data_to_string(Data) ->
    lists:flatten(io_lib:format("~w", [Data])).

string_to_data(String) when is_list(String) ->
    eval(lists:concat([String, "."]));

string_to_data(Binary) when is_binary(Binary) ->
    eval(binary_to_list(Binary) ++ ".").

reload_config(App, ConfigFile) when is_atom(App), is_list(ConfigFile)->
    case file:consult(ConfigFile) of
        {error,enoent} ->
            {ok, CWD} = file:get_cwd(),
            throw({error, enoent, CWD, ConfigFile});
        {ok, [{application,App,
            [{env, EnvTuples}]}]} ->
            [application:set_env(App, Key, Value) || {Key, Value} <- EnvTuples]
    end.

flatten_tuple_to_list(FieldValueList) ->
    flatten_tuple_to_list(FieldValueList, []).

flatten_tuple_to_list([], Acc) ->
    lists:reverse(Acc);
flatten_tuple_to_list([{Field, Value} | Rest], Acc) ->
    flatten_tuple_to_list(Rest, [Value, Field | Acc]).

%% 根据每个对象自身的权重值, 选择哪个对象
%% List: [{Weight, Obj}]
random_select(List) when is_list(List) ->
    TotalWeight = lists:sum([Weight || {Weight, _Obj} <- List]),
    Rand = m_rand:random(TotalWeight),
    random_select(List, Rand).

random_select([{Weight, Obj} | Rest], Rand) ->
    case Rand > Weight of
        true ->
            random_select(Rest, Rand - Weight);
        false -> Obj
    end.

random_one([]) ->
    throw({random_one_param_should_not_be_empty});
random_one(List) when is_list(List) ->
    lists:nth(m_rand:random(length(List)), List).

%% 随机选一个,一并返回剩下的
pick_one([]) ->
    throw({pick_one_param_should_not_be_empty});
pick_one(List) when is_list(List) ->
    One = lists:nth(m_rand:random(length(List)), List),
    Rest = lists:delete(One, List),
    {One, Rest}.

limit(Value, Min, Max) ->
    Temp1 = max(Min, Value),
    Temp2 = min(Max, Temp1),
    Temp2.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
utf8_string_length(String) when is_list(String) ->
    Bin = unicode:characters_to_binary(String),
    utf8_string_length(Bin);
utf8_string_length(Binary) when is_binary(Binary) ->
    %% UTF8格式说明 参考 http://www.ruanyifeng.com/blog/2007/10/ascii_unicode_and_utf-8.html
    L = binary_to_list(Binary),
    length([B || B <- L, (B >= 240) orelse          %% 长度为4字节的数量
    (B >= 224 andalso B < 240) orelse           %% 长度为3字节的数量
    (B >= 192 andalso B < 224) orelse           %% 长度为2字节的数量
    (B >= 0 andalso B < 128)]).                 %% 长度为1字节的数量

split(Bin, Char) when is_binary(Bin), is_integer(Char) ->
    split(binary_to_list(Bin), Char);
split([Char | Chars], Char) when is_list(Chars), is_integer(Char) ->
    split(Chars, Char);
split(Chars, Char) when is_list(Chars), is_integer(Char) ->
    Sag1 = lists:takewhile(fun(C) -> C =/= Char end, Chars),
    Rest = lists:dropwhile(fun(C) -> C =/= Char end, Chars),
    case Rest of
        [] ->
            [Sag1];
        [Char] ->
            [];
        [Char | Rest1] ->
            R = split(Rest1, Char),
            [Sag1 | R]
    end.

%% 将列表 List，分页长度为 SubLen 的子列表
%% splitlist(List, N) -> [List1, List2, ……]
%%     Types:
%%     N = integer() >= 0
%%      0..length(List)
%%      List = List1 = List2 = …… [T]
%%      T = term()
splitlist(List, N) when N > 0 ->
    Len = length(List),
    case Len of
        0 -> [];
        _ ->
            SubCount = (Len + N - 1) div N,
            [lists:sublist(List, (Idx * N) + 1, N) || Idx <- lists:seq(0, SubCount - 1)]
    end.

joinlist([H | Tail], Spliter) ->
    case Tail of
        [] ->
            "";
        _ ->
            H1 = case H of
                     _ when is_binary(H) ->
                         binary_to_list(H);
                     _ ->
                         H
                 end,
            lists:concat([H1, Spliter, joinlist(Tail, Spliter)])
    end.

-compile(export_all).
test_random_select() ->
    rand:start_link(),
    random_select([{5, a}, {5, b}, {5, c}, {12, d}]).


%% 检测L1中的所有元素是否在L2中都存在
check_list(L1, L2) ->
    Pred = fun(E) ->
        lists:member(E, L2)
    end,
    case lists:all(Pred, L1) of
        true -> true;
        false -> throw({not_match, L1, L2})
    end.

is_illegal_string(String) when is_binary(String)->
    lager:info("~p",[unicode:characters_to_list(String)]),
    length(lists:filter(fun(A)-> A>65535 end, unicode:characters_to_list(String))) > 0;
is_illegal_string(String) when is_list(String)->
    length(lists:filter(fun(A)-> A>65535 end, String)) > 0;
is_illegal_string(_) ->
    true.
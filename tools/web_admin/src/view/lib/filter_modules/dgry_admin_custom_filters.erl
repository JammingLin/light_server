-module(dgry_admin_custom_filters).
-compile(export_all).

% put custom filters in here, e.g.
%
% my_reverse(Value) ->
%     lists:reverse(binary_to_list(Value)).
%
% "foo"|my_reverse   => "oof"

datetime(TimeStamp) when is_integer(TimeStamp)->
    datetime:datetime_to_string(datetime:translate(datetime:timestamp_to_datetime(TimeStamp)));

datetime(DatetimeTuples) when is_list(DatetimeTuples) ->
%%    lager:info("datetime ~p", [DatetimeTuples]),
    Datetime = maps:from_list(DatetimeTuples),
    datetime:datetime_to_string(Datetime);
datetime(D) ->
%%    lager:info("datetime 2"),
    D.

news_area_name(AreaCode) ->
    Areas = dgry_admin_system_news_controller:handle_get_areas(),
%%    lager:info("Area ~p ~p", [AreaCode, Areas]),
    Filter = [Text || #{area := A, text := Text} <- Areas, A == AreaCode],
    case Filter of
        [AreaText] -> AreaText;
        _ -> "Unknow"
    end.

email_type_name(1) ->
    "Normal";
email_type_name(2) ->
    "Reward";
email_type_name(_TypeCode) ->
    "Other".

buy_info_id("hot_diamonds_tier1")->
    "90";
buy_info_id("hot_diamonds_tier5")->
    "500";
buy_info_id("hot_diamonds_tier10")->
    "1200";
buy_info_id("hot_diamonds_tier20")->
    "2500";
buy_info_id("hot_diamonds_tier50")->
    "6500";
buy_info_id("hot_diamonds_tier60")->
    "14000";

buy_info_id(<<"1">>)->
    "90";
buy_info_id(<<"2">>)->
    "500";
buy_info_id(<<"3">>)->
    "1200";
buy_info_id(<<"4">>)->
    "2500";
buy_info_id(<<"5">>)->
    "6500";
buy_info_id(<<"6">>)->
    "14000".

building_sum_trans(undefined)->
    0;
building_sum_trans(Val)->
    Val.

building_level(BuildingId)->
    BuildingId rem 100.

building_type(29)->
    unicode:characters_to_binary("守备营地");

building_type(15)->
    unicode:characters_to_binary("弓箭塔");

building_type(21)->
    unicode:characters_to_binary("高塔");

building_type(18)->
    unicode:characters_to_binary("炮塔");

building_type(22)->
    unicode:characters_to_binary("弩箭塔");

building_type(23)->
    unicode:characters_to_binary("火炮塔");

building_type(24)->
    unicode:characters_to_binary("火箭塔");

building_type(25)->
    unicode:characters_to_binary("弩炮塔");

building_type(26)->
    unicode:characters_to_binary("投石塔");

building_type(17)->
    unicode:characters_to_binary("火药桶");

building_type(30)->
    unicode:characters_to_binary("巨型炸弹");

building_type(13)->
    unicode:characters_to_binary("城墙").

building_name(Type)->
    building_type(Type).

news_type_name(1)->
    unicode:characters_to_binary("一般通知");
news_type_name(2)->
    unicode:characters_to_binary("维护通知");
news_type_name(3)->
    unicode:characters_to_binary("营销通知");
news_type_name(4)->
    unicode:characters_to_binary("新功能通知");
news_type_name(_TypeCode) ->
    unicode:characters_to_binary("其他").

selected(undefined, _) ->
    "";
selected(Value1, Value2) ->
    case list_to_binary(Value1) == Value2 of
        true -> "selected=\"selected\"";
        false ->
%%            io:format("selected:~p, ~p~n", [Value1, Value2]),
            "nono"
    end.

checked(undefined, _) ->
    "";
checked(Value1, Value2) ->
    case list_to_binary(Value1) == Value2 of
        true -> "checked=\"checked\"";
        false ->
%%            io:format("selected:~p, ~p~n", [Value1, Value2]),
            "nono"
    end.

add(Value, A) ->
    Value + A.

m100(Value) ->
    round(Value * 10000) / 100.

undefined_empty(undefined) ->
    "";
undefined_empty("undefined") ->
    "";
undefined_empty(Value) ->
    Value.

limit_len(Value, Len) ->
    Str = binary_to_list(Value),
    case length(Str) > Len of
        true ->
            string:sub_string(Str, 1, Len) ++ "......";
        false ->
            Value
    end.

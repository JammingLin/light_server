%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2015 下午2:24
%%%-------------------------------------------------------------------
-module(read_xml).
-author("linzhiwei").
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

parse(String) ->
    {{_,_,_,_,_,_,_,_,Records,_,_,_},_} = xmerl_scan:string(String),
    lists:flatten([begin extract(Record, []) end || Record <-Records]).

extract(Record, Acc) when is_record(Record, xmlElement) ->
    case Record#xmlElement.name of
        xmlElement ->
            ItemData = lists:foldl(fun extract/2, [], Record#xmlElement.content),
            [ {xmlElement, ItemData} | Acc ];
        _ ->
            lists:foldl(fun extract/2, Acc, Record#xmlElement.content)
    end;

extract({xmlText, [{Attribute, _}, {xml, _}], _, _, Value, _}, Acc) ->
    [{Attribute, Value}|Acc];

extract(_, Acc) ->
    Acc.
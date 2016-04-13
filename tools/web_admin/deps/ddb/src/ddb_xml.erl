%%% Copyright (C) 2012 Issuu ApS. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(ddb_xml).

%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Exported Functions
%%
-export([format/1,
         parse/1,
         format_attribute/2,
         format_attribute_full/2,
         format_text_element_full/2,
         parse_attribute/2,
         get_child/2,
         get_child_text/2,
         get_children/1,get_children/2,
         get_children_ns/2,
         get_text/1,
         get_node_name/1]).
-export([parse_json_att/2,
         format_json_att/2]).
%%
%% API Functions
%%
format(Xml) ->
    lists:flatten(xmerl:export_simple([Xml], utf8_export)).

parse(Bin) when is_binary(Bin) -> 
    parse(binary_to_list(Bin));
parse(XmlString) ->
    {Xml, _} = xmerl_scan:string(XmlString, [{encoding, "iso-10646-utf-1"}]),
    Xml.

format_attribute(_Name, undefined) -> [];
format_attribute(_Name, []) -> [];
format_attribute(Name, Value) -> {Name, Value}.

format_attribute_full(_Name, undefined) -> [];
format_attribute_full(_Name, []) -> [];
format_attribute_full(Name, Value) -> #xmlAttribute{name = Name, value = Value}.

format_text_element_full(_Name, undefined) -> [];
format_text_element_full(_Name, []) -> [];
format_text_element_full(Name, Value) -> #xmlElement{name = Name, content = [#xmlText{value = util:from_utf8(Value)}]}.

parse_attribute(Root, Attribute) ->
    case lists:keysearch(Attribute, 2, Root#xmlElement.attributes) of
        {value, Tuple} -> lists:flatten(Tuple#xmlAttribute.value);
        false -> undefined
    end.

get_child(Root, ChildName) ->
    case lists:keysearch(ChildName, 2, Root#xmlElement.content) of
        {value, Tuple} -> Tuple;
        false -> undefined
    end.

get_child_text(Root, ChildName) ->
    case lists:keysearch(ChildName, 2, Root#xmlElement.content) of
        {value, Tuple} ->
            lists:flatmap(fun(XmlText) ->
                XmlText#xmlText.value
            end, Tuple#xmlElement.content);
        false ->
            undefined
    end.

get_children(Root) -> get_children(Root, undefined). 
get_children(Root, Name) ->
    lists:filter(fun(Child) when is_record(Child, xmlElement) ->
        (Name == undefined) or (Child#xmlElement.name == Name);
                    (_Child) -> false
    end, Root#xmlElement.content).

get_children_ns(Root, NS) ->
    lists:filter(fun(Child) ->
        case Child#xmlElement.nsinfo of
            {NSName, _ElementName} -> NSName == NS;
            _ -> false
        end
    end, Root#xmlElement.content).

get_text(Root) ->
    lists:flatmap(fun(XmlText) ->
        XmlText#xmlText.value
    end, Root#xmlElement.content).

get_node_name(Element) -> Element#xmlElement.name.

%% JSON structs

parse_json_att(Key, {struct, Props}) when is_list(Props) ->
    case proplists:get_value(Key, Props) of
        null -> undefined;
        Other -> Other
    end.

format_json_att(_Name, undefined) -> [];
format_json_att(_Name, []) -> [];
format_json_att(Name, Value) -> {Name, Value}.
%%
%% Local Functions
%%


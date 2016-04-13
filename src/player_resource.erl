%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十一月 2015 下午5:01
%%%-------------------------------------------------------------------
-module(player_resource).
-author("linzhiwei").

-include("game_types.hrl").

%% API
-export([add_bean/2, add_exp/2, return_resource/2]).

add_bean(PlayerBean, Bean) when is_integer(PlayerBean), is_integer(Bean) ->
    PlayerBean + Bean;
add_bean(PlayerData, Bean) ->
    add_resource(PlayerData, [{bean, Bean}]).

add_exp(PlayerExp, Exp) when is_integer(PlayerExp), is_integer(Exp) ->
    PlayerExp + Exp;
add_exp(PlayerData, Exp) ->
    PlayerData1 = add_resource(PlayerData, [{experience, Exp}]),
    NewLevel = level:get_level(maps:get(experience, PlayerData1)),
    maps:put(level, NewLevel, PlayerData1).

return_resource(PlayerData, AddResources) ->
    add_resource(PlayerData, AddResources, true).

add_resource(PlayerData, AddResources) ->
    add_resource(PlayerData, AddResources, false).

add_resource(PlayerData, [], _IsReturn) ->
    PlayerData;
add_resource(#{player_id := _PlayerID} = PlayerData, [{_Resource, 0} | Rest], IsReturn) ->
    add_resource(PlayerData, Rest, IsReturn);
add_resource(#{player_id := _PlayerID} = PlayerData, [{Resource, Count} | Rest], IsReturn) ->
    NewCount = max(0,maps:get(Resource, PlayerData) + Count),
    PlayerData1 = maps:put(Resource, NewCount, PlayerData),
    if
        IsReturn == false ->
            ok;%notify_achievement(PlayerID, Resource, ActualCount);
        true -> ok
    end,
    add_resource(PlayerData1, Rest, IsReturn).
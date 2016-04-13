%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%  玩家等级
%%% @end
%%% Created : 25. 十一月 2014 16:15
%%%-------------------------------------------------------------------
-module(level).
-author("Administrator").

%% API
-export([get_level/1]).

get_level(Exp) when is_integer(Exp), Exp >= 0 ->
	L = tplt_data:get_all(level_exp),
	L1 = [Level || #{level:=Level, exp:=Exp1} <- L, Exp < Exp1],
	case L1 of
		[] ->
			[#{level:=Level} | _] = lists:sort(fun(#{level:=Lvl1}, #{level:=Lvl2}) -> Lvl1 >= Lvl2 end, L),
			Level;
		_ ->
			[MaxLevel | _] = lists:sort(L1),
			MaxLevel-1
	end;
get_level(PlayerData) ->
	Exp = maps:get(experience, PlayerData),
	get_level(Exp).

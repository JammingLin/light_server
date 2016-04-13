%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2015 上午10:00
%%%-------------------------------------------------------------------
-module(web_admin_statistics_controller, [Req, SessionID]).

-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

%% API
index('GET', []) ->
    {ok, []}.

question_error_rate('GET', [])->
    Rtn = handle_get_question_error_rate(),
    Result = [begin
                  Key = binary_to_list(maps:get(question_topic, QuestionInfo)),
                  [Company, TopicID] = string:tokens(Key, "@"),
                  #{
                      company=> Company,
                      topic_id=> TopicID,
                      question_id=> maps:get(question_id, QuestionInfo),
                      answer_times=> maps:get(answer_times, QuestionInfo),
                      error_rate => maps:get(error_rate, QuestionInfo)
                  }
              end || QuestionInfo <- Rtn],
    lager:info("question_error_report ~p", [Result]),
    ?REPLY_OK(Result).

question_error_report('GET', [])->
    Rtn = handle_get_question_error_report(),
    Result = [begin
              Key = binary_to_list(maps:get(question_topic, QuestionInfo)),
              [Company, TopicID] = string:tokens(Key, "@"),
              #{
                  company=> Company,
                  topic_id=> TopicID,
                  question_id=> maps:get(question_id, QuestionInfo),
                  error_report_times => maps:get(error_report_times, QuestionInfo)
                  }
              end || QuestionInfo <- Rtn],
    lager:info("question_error_report ~p", [Result]),
    ?REPLY_OK(Result).

get_newbie_guide_lost('GET', []) ->
    get_newbie_guide_lost('POST', []);
get_newbie_guide_lost('POST', [])->
    Days    = Req:post_param("day"),
    Channel = Req:post_param("channel"),
    io:format("~p", [Days]),
    case Days of
        "undefined" -> ?REPLY_OK([]);
        _ ->
            PassportCreateDate =  case Req:post_param("passport_create_date") of
                                      [] ->
                                          datetime:make_datetime(2015, 1, 1, 0, 0, 0);
                                      undefined ->
                                          datetime:make_datetime(2015, 1, 1, 0, 0, 0);
                                      "undefined" ->
                                          datetime:make_datetime(2015, 1, 1, 0, 0, 0);
                                      Txt ->
                                          datetime:datetime_from_string(Txt)
                                  end,
            Chnl = case Channel of
                "IOS"   -> ios;
                "MOBO"  -> mobo;
                "GOOGLE"-> google;
                 _ -> all
            end,
            #{player_count := PlayerCount, newbie_count := NewbieCount, newbie_data := NewbieData} = handle_get_player_lost_n_days(PassportCreateDate, list_to_integer(Days), Chnl),
            io:format("get_newbie_guide_lost ~p ~n", [NewbieData]),
            Data1 = [#{sub_step => SubStep, num => Num, all=> All, rate => Rate} || {SubStep, Num, All, Rate} <- NewbieData],
            io:format("data1 ~p~n", [Data1]),
            Data2 = lists:sort(fun(#{sub_step:=A}, #{sub_step:=B}) -> A < B end, Data1),
            io:format("data2 ~p~n", [Data2]),
            ?REPLY_OK(#{viewdata =>
            #{
                day => Days,
                channel => Channel,
                passport_create_date => datetime:date_to_string(PassportCreateDate)
            },
                rows => Data2, player_count => PlayerCount, newbie_count => NewbieCount })
%%            ?REPLY_OK(#{all => maps:get(all, hd(Data2)), rows => Data2})
    end.


handle_get_player_lost_n_days(Date, Days, Channel) ->
    rpc:call(web_admin_base:game_node(SessionID), statistics, player_lost_last_n_days, [Date, Days, Channel]).

handle_get_question_error_report()->
    rpc:call(web_admin_base:game_node(SessionID), statistics, question_error_report, []).

handle_get_question_error_rate()->
    rpc:call(web_admin_base:game_node(SessionID), statistics, question_error_rate, []).

pve_detail('GET', []) ->
    Levels = rpc_levels(),
    ?REPLY_OK(#{levels => Levels,
        items => handle_pve_detail(Levels)});
pve_detail('POST', []) ->
    Level = Req:post_param("sltLevel"),
    Levels = rpc_levels(),
    Items = handle_pve_detail(Levels, Level),
    Total = total_pve_details(Items),
    ?REPLY_OK(#{
        view_data => #{sltLevel => Level},
        levels => Levels,
        items => Items,
        total => Total}).

handle_pve_detail(Levels) ->
    Rows = rpc_statistics(pve_battle_info, []),
    [begin
         Win = maps:get(win, R),
         Lose = maps:get(lose, R),
         PveID = maps:get(pve_id, R),
         PveName = get_pve_name(Levels, PveID),
         R#{pve_name => PveName, times => Win + Lose,
             win_rate => rate(Win, Win + Lose),
             cost_time => maps:get(win_cost_time, R) + maps:get(lose_cost_time, R),
             all_rounds => maps:get(win_all_rounds, R) + maps:get(lose_all_rounds, R),
             open_recommend_times => maps:get(open_recommend_times, R)}
     end || R <- Rows].

handle_pve_detail(Levels, Level) ->
    Rows = case Level of
               "" ->
                   rpc_statistics(pve_battle_info, []);
               _ ->
                   rpc_statistics(pve_battle_info, [Level])
           end,
    [begin
         Win = maps:get(win, R),
         Lose = maps:get(lose, R),
         WinCT = maps:get(win_cost_time, R),
         LoseCT = maps:get(lose_cost_time, R),
         PveID = maps:get(pve_id, R),
         PveName = get_pve_name(Levels, PveID),
         R#{pve_name => PveName, times => Win + Lose,
             win_rate => rate(Win, Win + Lose),
             cost_time_avg => dv(WinCT + LoseCT, Win + Lose),
             win_cost_time_avg => dv(WinCT, Win),
             lose_cost_time_avg => dv(LoseCT, Lose),
             all_rounds => maps:get(win_all_rounds, R) + maps:get(lose_all_rounds, R)}
     end || R <- Rows].

total_pve_details(Details) ->
    Len = length(Details),
    Fields = [win, lose, times, win_rate, cost_time_avg, win_cost_time_avg,
        lose_cost_time_avg, all_rounds, win_all_rounds, lose_all_rounds],

    FoldFields = fun(Field, {Item, SumTemp}) ->
        OldValue = maps:get(Field, SumTemp, 0),
        ItemValue = maps:get(Field, Item),
        {Item, maps:put(Field, OldValue + ItemValue, SumTemp)}
                 end,

    FoldDetails = fun(Item, Sum) ->
        {Item, Sum1} = lists:foldl(FoldFields, {Item, Sum}, Fields),
        Sum1
                  end,

    Total = lists:foldl(FoldDetails, #{}, Details),

    FoldDiv = fun(Field, {Length, Data}) ->
        {Length, maps:update(Field, dv(maps:get(Field, Data), Length), Data)}
              end,
    {Len, T1 } = lists:foldl(FoldDiv, {Len, Total}, [win_rate, cost_time_avg, win_cost_time_avg,
        lose_cost_time_avg, all_rounds, win_all_rounds, lose_all_rounds]),
    T1.

lost_player_info('GET', []) ->
    Days = Req:query_param("days"),
    Rows = rpc_statistics(lost_player_info, [list_to_integer(Days)]),
    lager:error("rows sssssssssss  :~p", [Rows]),
    ?REPLY_OK(Rows).

level_distribution('GET', []) ->
    D = #{day => "2015-09-08", d1 => 0, d2 => 0, d3 => 0, d4 => 0, d5 => 0,
        d6 => 0, d7 => 0, d8 => 0, d9 => 0, d10 => 10, d11 => 0, d12 => 0},
    Ret = [D, D, D, D, D, D, D],
    ?REPLY_OK(Ret).


trans_building_sum(1, Num, Map)->
    Map #{building_sum1=>Num};
trans_building_sum(2, Num, Map)->
    Map #{building_sum2=>Num};
trans_building_sum(3, Num, Map)->
    Map #{building_sum3=>Num};
trans_building_sum(4, Num, Map)->
    Map #{building_sum4=>Num};
trans_building_sum(5, Num, Map)->
    Map #{building_sum5=>Num};
trans_building_sum(6, Num, Map)->
    Map #{building_sum6=>Num};
trans_building_sum(7, Num, Map)->
    Map #{building_sum7=>Num};
trans_building_sum(8, Num, Map)->
    Map #{building_sum8=>Num};
trans_building_sum(9, Num, Map)->
    Map #{building_sum9=>Num};
trans_building_sum(10, Num, Map)->
    Map #{building_sum10=>Num};
trans_building_sum(11, Num, Map)->
    Map #{building_sum11=>Num};
trans_building_sum(12, Num, Map)->
    Map #{building_sum12=>Num};
trans_building_sum(13, Num, Map)->
    Map #{building_sum13=>Num};
trans_building_sum(14, Num, Map)->
    Map #{building_sum14=>Num};
trans_building_sum(15, Num, Map)->
    Map #{building_sum15=>Num};
trans_building_sum(16, Num, Map)->
    Map #{building_sum16=>Num};
trans_building_sum(17, Num, Map)->
    Map #{building_sum17=>Num};
trans_building_sum(18, Num, Map)->
    Map #{building_sum18=>Num};
trans_building_sum(19, Num, Map)->
    Map #{building_sum19=>Num};
trans_building_sum(20, Num, Map)->
    Map #{building_sum20=>Num}.


activity_building_detail_info('GET', []) ->
    Record = rpc_statistics(get_building_detail_info, []),
    Ret = lists:foldl(fun({{SL, Type, Bid}, Num}, T) ->
        case lists:keyfind({SL, Type}, 1, T) of
            false ->
                lists:append(T, [{{SL, Type},trans_building_sum(Bid rem 100, Num, #{strong_level=>SL, building_type=>Type})}]);
            {Key, Map} ->
                lists:keyreplace(Key,1, T, {Key,trans_building_sum(Bid rem 100, Num, Map)})
        end
                      end, [], Record),
    Record1 = [begin MapInfo end || {_, MapInfo}<-Ret],
    ?REPLY_OK(Record1).

pve_refresh_info('GET', []) ->
    Levels = rpc_levels(),
    Rows = rpc_statistics(pve_refresh_info, []),
    Rows1 = [begin
                 PveID = maps:get(pve_id, R),
                 PveName = get_pve_name(Levels, PveID),
                 R#{pve_name => PveName}
             end || R <- Rows],
    ?REPLY_OK(#{rows => Rows1, levels => Levels}).

player_level_up_battle_info('GET', []) ->
    Rows = rpc_statistics(player_level_up_battle_info, []),
    lager:error("player_level_up_battle_info sssssssssss  :~p", [Rows]),
    Rows1 = [V#{level => Level} || #{battleInfo := V, level := Level} <- Rows],
    ?REPLY_OK(Rows1).

pvp_battle_info('GET', []) ->
    Rows = rpc_statistics(pvp_battle_info, []),
    lager:error("pvp_battle_info sssssssssss  :~p", [Rows]),
    ?REPLY_OK(Rows).

battle_function('GET', []) ->
    Day1 = Req:query_param("start_day"),
    Day2 = Req:query_param("end_day"),
    Rows = rpc_statistics(withdraw_battle_info, ["2015-1-9 0:0:0", "2017-9-9 23:59:59"]),
    lager:error("battle_function sssssssssss  :~p", [Rows]),

    Rows1 = [begin
                 R#{
                     all_av_withdraw_rate => dv(PveWTimes + PvpWTimes, PveBTimes + PvpBTimes),
                     pve_av_withdraw_rate => dv(PveWTimes, PveBTimes),
                     pvp_av_withdraw_rate => dv(PvpWTimes, PvpBTimes),

                     all_av_use_auto_battle_rate => dv(PveATimes + PvpATimes, PveBTimes + PvpBTimes),
                     pve_av_use_auto_battle_rate => dv(PveATimes, PveBTimes),
                     pvp_av_use_auto_battle_rate => dv(PvpATimes, PvpBTimes)
                 }
             end || #{pve_battle_times := PveBTimes,
        pve_use_auto_battle_times := PveATimes,
        pve_withdraw_times := PveWTimes,
        pvp_battle_times := PvpBTimes,
        pvp_use_auto_battle_times := PvpATimes,
        pvp_withdraw_times := PvpWTimes} = R <- Rows],

    ?REPLY_OK(Rows1).

rpc_levels() ->
    Levels = rpc(game, tplt_data, get_all, [level]),
    lists:sort(fun(#{id:=Id1}, #{id:=Id2}) -> Id1 < Id2 end, Levels).
get_pve_name(Levels, PveID) ->
    [Name] = [CNName || #{id := ID, cn_name := CNName} <- Levels, ID =:= PveID],
    Name.

% -------

rate(_Num1, 0) ->
    0;
rate(Num1, Num2) ->
    erlang:round(Num1 * 10000 / Num2) / 100.


dv(_Num1, 0) ->
    0;
dv(Num1, Num2) ->
    erlang:round(Num1 * 100 / Num2) / 100.

rpc(game, Mod, Fun, Args) ->
    rpc:call(web_admin_base:game_node(SessionID), Mod, Fun, Args).

rpc_statistics(Fun, Args) ->
    rpc(game, statistics, Fun, Args).


analyse_retention('GET', []) ->
    StartDay = Req:query_param("start_day"),
    EndDay = Req:query_param("end_day"),
    UseNewbie = Req:query_param("use_newbie"),
    Module = case UseNewbie of
               "1" ->
                   sts_retention_newbie;
               _ ->
                   sts_retention
           end,
    Data = case EndDay of
               "undefined" ->
                   [];
               [] ->
                   [];
               _ ->
                R = rpc(game, Module, analyse, [StartDay, EndDay]),
                lager:debug("r:~p", [R]),
                R
           end,
    ?REPLY_JSON(Data).

retention('GET', []) ->
    retention('POST', []);
retention('POST', []) ->
    Areas = handle_get_areas(),
    StartDay = Req:post_param("start_day"),
    EndDay = Req:post_param("end_day"),
    Mode = Req:post_param("mode"),
    Area = Req:post_param("area"),
    Channel = Req:post_param("channel"),
    UseNewbie = Req:post_param("use_newbie"),
    lager:info("usen:~p", [UseNewbie]),
    Module = case UseNewbie of
                 "1" ->
                     sts_retention_newbie;
                 _ ->
                     sts_retention
             end,
    Data = case EndDay of
               "undefined" ->
                   [];
               [] ->
                   [];
               _ ->
                   handle_retention(Module, StartDay, EndDay, Mode, Area, Channel)
           end,

    Now = datetime:localtime(),
    InitStart = datetime:date_to_string(datetime:add_datetime(Now, -7 * 86400)),
    InitEnd = datetime:date_to_string(datetime:add_datetime(Now, -1 * 86400)),
    ?REPLY_OK(#{viewdata =>
        #{
            areas => Areas,
            start_day => if_empty(StartDay, InitStart),
            end_day => if_empty(EndDay, InitEnd),
            mode => Mode,
            area => Area,
            channel => Channel,
            use_newbie => UseNewbie
        },
        rows => Data}).

handle_retention(Module, Day, Day, Mode, Area, Channel) ->
    Data = rpc(game, Module, handle_get_report, [Day, Mode, Area, Channel]),
    lager:debug("data:~p", [Data]),
    Data;
handle_retention(Module, SDay, EDay, Mode, Area, Channel) ->
    Data = rpc(game, Module, handle_get_report, [SDay, EDay, Mode, Area, Channel]),
    lager:debug("data:~p", [Data]),
    Data.

handle_get_areas() ->
    Ret   = web_admin_base:gs_rpc(SessionID, tplt_data, get_all, [area]),
    SortFun = fun(A, B) ->
        maps:get(text, A) < maps:get(text, B)
              end,

    Areas = [#{
        area => binary_to_list(maps:get(ab, Area)),
        text =>
        io_lib:format("~s ~s", [
            binary_to_list(maps:get(country_name_en, Area)),
            binary_to_list(maps:get(country_name_cn, Area))
        ])
    }
        || Area <- Ret, binary_to_list(maps:get(ab, Area)) =/= []],

    SortAreas = lists:sort(SortFun, Areas),
    SortAreas.

if_empty(undefined, V) ->
    V;
if_empty("undefined", V) ->
    V;
if_empty([], V) ->
    V;
if_empty(O, _) ->
    O.
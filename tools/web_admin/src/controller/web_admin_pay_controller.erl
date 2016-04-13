%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2015 下午8:05
%%%-------------------------------------------------------------------
-module(web_admin_pay_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

-define(MOBO_APP_ID, 351).
-define(MOBO_APP_KEY, "0C60AD2DBAD9CD44973D2501E734D494").
-define(MOBO_SECRET_KEY, "5D7E89AA15238A428ED06B77301C74CA").

before_("mobo_pay", _, _) ->
    ok;
before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    PayList = handle_get_all_pay_list(),
    lager:debug("PayList:~p", [PayList]),
    ?REPLY_OK(PayList);

index('POST', []) ->
    PlayerID = Req:post_param("player_id"),
    case PlayerID of
        [] -> ?REPLY_OK(handle_get_all_pay_list());
        _ ->
            Emails = handle_get_player_pay_list(PlayerID),
            ?REPLY_OK(Emails)
    end.


handle_get_all_pay_list()->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_pay, get_all_pay_list, []),
    Ret.

handle_get_player_pay_list(PlayerID)->
    Ret = rpc:call(web_admin_base:game_node(SessionID), player_pay, get_player_pay_list, [PlayerID]),
    Ret.




google_index('GET', [])->
    PayList = handle_get_all_google_pay_list(),
    lager:debug("PayList:~p", [PayList]),
    ?REPLY_OK(PayList);

google_index('POST', []) ->
    PlayerID = Req:post_param("player_id"),
    case PlayerID of
        [] -> ?REPLY_OK(handle_get_all_google_pay_list());
        _ ->
            Emails = handle_get_player_google_pay_list(PlayerID),
            ?REPLY_OK(Emails)
    end.

handle_get_all_google_pay_list()->
    Ret = rpc:call(web_admin_base:game_node(SessionID), google_pay, get_all_pay_list, []),
    Ret.

handle_get_player_google_pay_list(PlayerID)->
    Ret = rpc:call(web_admin_base:game_node(SessionID), google_pay, get_player_pay_list, [PlayerID]),
    Ret.

mobo_pay('GET', []) ->
    mobo_pay('POST', []);
mobo_pay('POST', []) ->
    SuccessReturnSign = md5_string:md5_hex(lists:concat([?MOBO_APP_ID, 1, ?MOBO_SECRET_KEY])),
    FailReturnSign = md5_string:md5_hex(lists:concat([?MOBO_APP_ID, 0, ?MOBO_SECRET_KEY])),

    try
        AppID = list_to_integer(Req:post_param("AppID")),
        OrderSerial = Req:post_param("OrderSerial"),
        CPOrderSerial = Req:post_param("CooperatorOrderSerial"),
        OrderSign = Req:post_param("Sign"),
        Content_O = Req:post_param("Content"),
        Content = re:replace(Content_O, " ", "+", [global, {return,list}]),

        lager:debug("~p, ~p, ~p, ~p, ~p", [AppID, OrderSerial, CPOrderSerial, OrderSign, Content]),

        mobo_pay_check_app_id(?MOBO_APP_ID, AppID),
        mobo_pay_check_order_sign(AppID, OrderSerial, CPOrderSerial, Content, OrderSign),

        {ContentString, ContentData} = mobo_pay_decode_content(Content),
        UID = maps:get('UID', ContentData),
        #{redirect:=Rdir, test_uids:=TestUids} = get_mobo_pay_test_config(),
        case TestUids == [-999] orelse lists:member(UID, TestUids) of
            true ->
                PostData = lists:concat(["AppID=", AppID, "&OrderSerial=", OrderSerial,
                    "&CooperatorOrderSerial=", CPOrderSerial,
                    "&Sign=", OrderSign, "&Content=", Content]),
                JsonString = utils:http_post(Rdir, PostData, 10000),
                lager:info("redirect return :~p", [JsonString]),
                Data = json:decode_to_map(JsonString),
                {json, maps:to_list(Data)};
            false ->
                case mobo_pay_notify_game(OrderSerial, CPOrderSerial, maps:get('OrderStatus', ContentData), ContentString) of
                    ok ->
                        {json, [{"AppID", integer_to_list(?MOBO_APP_ID)},
                            {"ResultCode", 1},
                            {"ResultMsg", "Success"},
                            {"Sign", SuccessReturnSign}]
                        };
                    Other ->
                        lager:warning("mobo_pay_notify:~p", [Other]),
                        {json, [{"AppID", integer_to_list(?MOBO_APP_ID)},
                            {"ResultCode", 0},
                            {"ResultMsg", "Fail"},
                            {"Sign", FailReturnSign}]
                        }
                end
        end
    catch
        C:R ->
            S = erlang:get_stacktrace(),
            lager:error("mobo_pay:~p, ~p, ~p", [C, R, S]),
            {json, [{"AppID", integer_to_list(?MOBO_APP_ID)},
                {"ResultCode", 0},
                {"ResultMsg", "Fail"},
                {"Sign", FailReturnSign}]
            }
    end.

get_mobo_pay_test_config() ->
    case application:get_env(dgry_admin, mobo_pay_test_config) of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            case file:read_file(Cwd ++ "/mobo_pay_test_config") of
                {ok, Bin} ->
                    Data = utils:binary_to_data(Bin),
                    application:set_env(dgry_admin, mobo_pay_test_config, Data),
                    Data;
                {error, _} ->
                    Data = #{redirect => "", test_uids => []},
                    application:set_env(dgry_admin, mobo_pay_test_config, Data),
                    Data
            end;
        {ok, D} ->
            D
    end.

mobo_pay_check_app_id(MoboAppID, AppID) ->
    case MoboAppID of
        AppID ->
            ok;
        _ ->
            throw({invalid_app_id, MoboAppID})
    end.

mobo_pay_check_order_sign(AppID, OrderSerial, CooperatorOrderSerial, Content, OrderSign) ->
    ExpectSign = md5_string:md5_hex(lists:concat([AppID, OrderSerial, CooperatorOrderSerial, Content, ?MOBO_SECRET_KEY])),
    case OrderSign of
        ExpectSign ->
            ok;
        _ ->
            throw({invalid_order_sign, ExpectSign})
    end.

mobo_pay_decode_content(Content) ->
    Content1 = base64:decode(Content),
    {Content1, json:decode_to_map(Content1)}.

mobo_pay_notify_game(OrderSerial, CPOrderSerial, OrderStatus, ContentString) ->
    {ok, GameNode} = application:get_env(dgry_admin, mobo_pay_callback_node),
    rpc:call(GameNode, pay, mobo_pay, [OrderSerial, CPOrderSerial, OrderStatus, ContentString], 10000).

query_mobo_pay_status('GET', []) ->
    CPOrderID = Req:query_param("CPOrderID"),
    Ret = web_admin_base:gs_rpc(SessionID, pay, query_mobo_pay_status, [CPOrderID]),
    ?REPLY_JSON(Ret).

mobo_index('GET', []) ->
    ?REPLY_OK([]);

mobo_index('POST', []) ->
    PageNo = list_to_integer(Req:post_param("page_no")),
    PageSize = list_to_integer(Req:post_param("page_size")),
    Status = utils:safe_sql(Req:post_param("status")),
    PlayerID = utils:safe_sql(Req:post_param("player_id")),

    SearchParams = #{
        page_no => PageNo,
        page_size => PageSize,
        status => Status,
        player_id => PlayerID
    },
    #{total:=T, rows:=R} = handle_get_mobo_pay_list(SearchParams),
    Return = #{
        rows => R,
        pager => #{
            total => T,
            page_no => PageNo,
            page_size => PageSize,
            page_count => (T + PageSize - 1) div PageSize
        },
        viewdata => SearchParams
    },
    ?REPLY_JSON(Return).


handle_get_mobo_all_pay_list()->
    Ret = rpc:call(web_admin_base:game_node(SessionID), pay, get_all_mobo_pay_list, []),
    Ret.

handle_get_mobo_pay_list(SearchParams)->
    Ret = rpc:call(web_admin_base:game_node(SessionID), pay, search_mobo_pay_list, [SearchParams]),
    Ret.

handle_get_mobo_player_pay_list(PlayerID)->
    Ret = rpc:call(web_admin_base:game_node(SessionID), pay, get_mobo_pay_list, [PlayerID]),
    Ret.


%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2015 下午5:46
%%%-------------------------------------------------------------------
-module(web_admin_system_news_controller, [Req, SessionID]).
-author("chendh").

-compile(export_all).
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    Areas = handle_get_areas(),
    News = [],
    ?REPLY_OK(#{areas => Areas, news => News});

index('POST', []) ->
    Areas = handle_get_areas(),
    AreaCode = Req:post_param("area_code"),
    Title = Req:post_param("title"),
    News = handle_lookup_news(AreaCode, Title),
    ?REPLY_OK(#{areas => Areas,
        news => News,
        viewbag => #{
            area_code => AreaCode,
            title => Title
        }}).

detail('GET', []) ->
    Areas = handle_get_areas(),
    Area     = Req:query_param("area_id"),
    NewsId   = Req:query_param("news_id"),
    NewsInfo = handle_get_news_info(Area, list_to_integer(NewsId)),
    lager:info("detail news info ~p ~p ~p", [Area, NewsInfo, NewsInfo]),
    ?REPLY_OK(#{areas => Areas, news_info=>NewsInfo});

detail('POST', [])->
    try
        lager:info("detail : ~p", [Req:post_param("txt_sendtime")]),
        Ret = handle_update_news(
            Req:post_param("area"),
            list_to_integer(Req:post_param("news_id")),
            list_to_integer(Req:post_param("type")),
            list_to_binary(Req:post_param("title")),
            list_to_binary(Req:post_param("news_icon")),
            list_to_binary(Req:post_param("content")),
            list_to_binary(Req:post_param("url")),
            datetime:datetime_from_string(Req:post_param("txt_sendtime")),
            datetime:datetime_from_string(Req:post_param("txt_deadline"))),
        ?REPLY_OK(Ret)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            lager:error("Class:~p, Reason:~p, Stacktrace:~p", [Class, Reason, Stacktrace]),
            ?REPLY_OK({error, 500, ""})
    end.

get_areas('GET', []) ->
    Areas = handle_get_areas(),
    ?REPLY_JSON(Areas).

add('GET', []) ->
    Areas = handle_get_areas(),
    {ok, Cwd}  = file:get_cwd(),
    {ok, Dirs} =  file:list_dir(Cwd ++ "/priv/static/icons-v1"),
    AllIcons = lists:foldl(fun(Dir, T)->
        {ok, Subdirs} = file:list_dir(Cwd ++ "/priv/static/icons-v1/" ++ Dir),
        lists:foldl(fun(Icon, T2)->
                        [IconName, Suffix] = string:tokens(Icon, "."),
                        lists:append(T2, [#{icon=>IconName, icon_dir=>lists:concat([Dir, "/", IconName]), subffix=>Suffix}])
                    end, T, Subdirs)
                           end, [], Dirs),
    lager:info("Icon list ~p", [AllIcons]),
    {ok, [{areas, Areas},
        {icons, AllIcons}
    ]};

add('POST', []) ->
    lager:info("content:~p", [Req:post_param("content")]),
    try
        Area = Req:post_param("area"),
        lager:info("Area:~p", [Area]),
        Areas = utils:split(Area, $,),
        Rets = [handle_save_news(
            A,
            list_to_integer(Req:post_param("type")),
            list_to_binary(Req:post_param("title")),
            list_to_binary(Req:post_param("icon")),
            list_to_binary(Req:post_param("content")),
            list_to_binary(Req:post_param("url")),
            datetime:datetime_from_string(Req:post_param("sendtime")),
            datetime:datetime_from_string(Req:post_param("deadline"))) || A <- Areas],
        ?REPLY_OK(Rets)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            lager:error("Class:~p, Reason:~p, Stacktrace:~p", [Class, Reason, Stacktrace]),
            ?REPLY_OK({error, 500, ""})
    end.


delete('GET', []) ->
    Area    = Req:query_param("area_id"),
    NewsId  = Req:query_param("news_id"),
    Ret = #{area => Area},
    handle_delete_news(Area, list_to_integer(NewsId)),
    ?REPLY_OK(Ret).

delete_news_by_title('GET', []) ->
    NewsTitle  = Req:query_param("news_title"),
    Ret = handle_delete_news_by_title(NewsTitle),
    ?REPLY_OK(Ret).

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
%%     Areas = [#{area => "0", text => "All"},
%%         #{area => "CN", text => "PRC of China"},
%%         #{area => "HK", text => "Hong Kong"},
%%         #{area => "TW", text => "Taiwan"},
%%         #{area => "AU", text => "Australia"},
%%         #{area => "US", text => "United States of America"},
%%         #{area => "MO", text => "Macao"}
%%     ],
    SortAreas.

handle_lookup_news(AreaCode, Title) ->
    lager:debug("handle_lookup_news:~p, ~p", [AreaCode, Title]),
    Ret = web_admin_base:gs_rpc(SessionID, system_news, lookup_news, [AreaCode, Title]),
    Ret.

handle_save_news(Area, Type, Title, NewsIcon, Content, Url, SendTime, DeadLine) ->
    Ret = web_admin_base:gs_rpc(SessionID, system_news, add_news, [Area, Type, Title, NewsIcon, Content, Url, SendTime, DeadLine]),
    Ret.

handle_update_news(Area, NewsId, Type, Title, NewsIcon, Content, Url, SendTime, DeadLine) ->
    NewsInfo = handle_get_news_info(Area, NewsId),
    New = NewsInfo #{type=>Type, title=>Title, news_icon => NewsIcon, content=>Content, url=>Url, send_time=>SendTime, dead_line=>DeadLine},
    Ret = web_admin_base:gs_rpc(SessionID, system_news, update_news, [New]),
    Ret.

handle_get_news_info(Area, NewsId)->
    Ret = web_admin_base:gs_rpc(SessionID, system_news, get_news_info, [Area, NewsId]),
    Ret.

handle_delete_news(Area, NewsId) ->
    Ret = web_admin_base:gs_rpc(SessionID, system_news, delete_news_by_newsid, [Area, NewsId]),
    Ret.

handle_delete_news_by_title(NewsTitle) ->
    SafeTitle = utils:safe_sql(NewsTitle),
    Ret = web_admin_base:gs_rpc(SessionID, system_news, delete_news_by_title, [SafeTitle]),
    Ret.
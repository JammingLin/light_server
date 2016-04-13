%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2015 下午4:01
%%%-------------------------------------------------------------------
-module(web_admin_testconfig_controller, [Req, SessionID]).
-author("chendh").
-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

%% API
-compile(export_all).

index('GET', []) ->
    {ok, []}.

mobo_pay_test_config('GET', []) ->
    Data = handle_get_mobo_test_config(),
    ?REPLY_OK(Data);

mobo_pay_test_config('POST', []) ->
    Redirect = Req:post_param("redirect"),
    UidsString = Req:post_param("test_uids"),
    ok = handle_save_mobo_test_config(Redirect, UidsString),
    Data = handle_get_mobo_test_config(),
    ?REPLY_OK(Data).

handle_get_mobo_test_config() ->
    {ok, Cwd} = file:get_cwd(),
    File = lists:concat([Cwd, "/", mobo_pay_test_config]),
    lager:debug("file" ++ File),
    {ok, TextBin} = file:read_file(File),
    #{redirect := Redirect, test_uids := Uids} = utils:binary_to_data(TextBin),
    #{redirect => Redirect, test_uids => string:join([integer_to_list(I) || I <- Uids], ",")}.

handle_save_mobo_test_config(Redirect, UidsString) ->
    Uids = [list_to_integer(I) || I <- string:tokens(UidsString, ",")],
    Data = #{redirect => Redirect, test_uids => Uids},
    Bin = utils:data_to_binary(Data),
    lager:debug("Bin:~p", [Bin]),
    {ok, Cwd} = file:get_cwd(),
    File = lists:concat([Cwd, "/", mobo_pay_test_config]),
    application:set_env(dgry_admin, mobo_pay_test_config, Data),
    ok = file:write_file(File, Bin).
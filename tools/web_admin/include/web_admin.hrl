%%%-------------------------------------------------------------------
%%% @author chendh
%%% @copyright (C) 2015, nd.com.cn
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2015 下午12:48
%%%-------------------------------------------------------------------
-author("chendh").

-define(GAME_NODE, fun() -> case application:get_env(dgry_admin, dgry_game_node) of
                                {ok, Value} -> Value;
                                _ -> unknown
                            end end()).

-define(GATE_NODE, fun() -> case application:get_env(dgry_admin, dgry_gate_node) of
                                {ok, Value} -> Value;
                                _ -> unknown
                            end end()).

-define(DB_NODE, fun() -> case application:get_env(dgry_admin, dgry_db_node) of
                              {ok, Value} -> Value;
                              _ -> unknown
                          end end()).

-define(REPLY_JSON, fun(Data) ->
    case Data of
        {error, Code, Msg} ->
            {json, [{code, Code}, {msg, Msg}, {data, null}]};
        undefined ->
            {json, [{code, 1}, {msg, ""}, {data, null}]};
        _ ->
            ExpData = expend_map(Data),
            {json, maps:to_list(#{code => 1, msg => "ok", data => ExpData})}
    end
                    end).

-define(REPLY_OK, fun(Data) ->
    case Data of
        {error, Code, Msg} ->
            {ok, [{code, Code}, {msg, Msg}, {data, null}]};
        {error, Code, Msg, Stack} ->
            {ok, [{code, Code}, {msg, Msg}, {data, null}, {stacktrace, Stack}]};
        undefined ->
            {ok, [{code, 1}, {msg, ""}, {data, null}]};
        _ ->
            ExpData = expend_map(Data),
            {ok, maps:to_list(#{code => 1, msg => "ok", data => ExpData})}
    end
                  end).

-define(REPLY_ERROR, fun({Msg}) ->
    io:format("~p ~n", [Msg]),
        {ok, [{code, 0}, {msg, Msg}, {data, null}]};
    ({Msg, Data}) ->
        io:format("~p ~n", [Msg]),
        {ok, [{code, 0}, {msg, Msg}, {data, Data}]}
                  end).
expend_map({Atom, Data}) when is_atom(Atom) ->
    {Atom, expend_map(Data)};
expend_map(Data) when is_map(Data)->
    List = maps:to_list(Data),
    [expend_map(Item) || Item <- List];
expend_map(Data) when is_list(Data)->
    [expend_map(L) || L <- Data];
expend_map(Data) ->
    Data.


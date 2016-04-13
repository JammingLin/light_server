%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%  事件系统, 该系统只能用在单进程的事件注册和触发上
%%% @end
%%% Created : 10. 十二月 2014 11:22
%%%-------------------------------------------------------------------
-module(event).
-author("Administrator").

%% API
-export([listen/3, cancel_listen/3, send/1, send/2]).
-export([test_events/0, test_events/1]).

%% 注册监听指定的事件
listen(EventName, Module, Function) ->
	Events1 = case get(events) of
		undefined ->
			maps:put(EventName, [{Module, Function}], #{});
		Events ->
			EventList = maps:get(EventName, Events, []),
			%% 需要判定指定的函数是否已经存在, 重复注册没意义, 且容易产生bug
			case is_exist(Module, Function, EventList) of
				true -> ok;
				false ->
					EventList1 = lists:append(EventList, [{Module, Function}]),
					maps:put(EventName, EventList1, Events)
			end
	end,
	put(events, Events1),
	ok.

is_exist(Module, Function, EventList) ->
	lists:any(fun({M, F}) -> Module==M andalso F == Function end, EventList).

%% 取消监听具体的事件
cancel_listen(EventName, Module, Function) ->
	case get(events) of
		undefined -> throw({no_event, EventName, Module, Function});
		Events ->
			EventList = maps:get(EventName, Events),
			%% 找到对应的函数, 并且删除掉
			EventList1 = delete(Module, Function, EventList, []),
			Events1 = maps:put(EventName, EventList1, Events),
			put(events, Events1),
			ok
	end.

delete(_Module, _Function, [], Result) ->
	lists:reverse(Result);
delete(Module, Function, [{Module, Function} | Rest], Result) ->
	delete(Module, Function, Rest, Result);
delete(Module, Function, [{_, _}=MF | Rest], Result) ->
	delete(Module, Function, Rest, [MF | Result]).

%% 发送具体的事件
send(EventName) ->
	send(EventName, []).
send(EventName, Params) ->
	case get(events) of
		undefined ->
            lager:debug("undifend event:~p,~p in ~p", [EventName, Params, get(events)]);
		Events ->
			EventList = maps:get(EventName, Events, []),
			[apply(Module, Function, Params) || {Module, Function} <- EventList]
	end,
	ok.

test_events() ->
	io:format("test_events~n").
test_events(Params) ->
	io:format("test_events, params:~p~n", [Params]).





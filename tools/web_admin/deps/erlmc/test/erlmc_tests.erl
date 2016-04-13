-module(erlmc_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, "127.0.0.1").
-define(PORT, 11211).
-define(LOCALHOST, {"127.0.0.1", 11211, 5}).

basic_memcached_test_() ->
    {foreach,
        spawn,
        fun basic_tests_setup/0,
        fun basic_tests_cleanup/1,
        [
            fun set_and_get_t/1,
            fun add_already_exists_t/1,
            fun append_t/1,
            fun prepend_t/1,
            fun delete_t/1,
            fun multi_get_t/1,
            fun stats_t/1,
            fun add_remove_server_t/1,
            fun set_and_get_timeout_t/1
        ]
    }.

basic_tests_setup() ->
    {ok, Pid} = erlmc:start_link([?LOCALHOST]),
    Pid.

basic_tests_cleanup(Pid) ->
    erlmc:flush(0),
    unlink(Pid),
    exit(Pid, shutdown),
    Mon = monitor(process, Pid),
    receive
        {'DOWN', Mon, process, _Pid, _Reason} -> ok
    after
        5000 -> timeout
    end.

set_and_get_t(_Setup) ->
    [?_assertEqual(<<>>, erlmc:set("Hello", <<"World">>)),
	 ?_assertEqual(<<"World">>, erlmc:get("Hello"))].

add_already_exists_t(_Setup) ->
    erlmc:set("Hello", <<"World">>),
    ?_assertEqual(<<"Data exists for key.">>, erlmc:add("Hello", <<"Fail">>)).

append_t(_Setup) ->
    erlmc:set("Hello", <<"World">>),
	[?_assertEqual(<<>>, erlmc:append("Hello", <<"!!!">>)),
     ?_assertEqual(<<"World!!!">>, erlmc:get("Hello"))].

prepend_t(_Setup) ->
    erlmc:set("Hello", <<"World">>),
	[?_assertEqual(<<>>, erlmc:prepend("Hello", <<"!!!">>)),
     ?_assertEqual(<<"!!!World">>, erlmc:get("Hello"))].

delete_t(_Setup) ->
    erlmc:set("Hello", <<"World">>),
	[?_assertEqual(<<>>, erlmc:delete("Hello")),
	 ?_assertEqual(<<>>, erlmc:get("Hello"))].

multi_get_t(_Setup) ->
    erlmc:set("One", <<"A">>),
    erlmc:set("Two", <<"B">>),
    erlmc:set("Three", <<"C">>),
    GetManyResponse = erlmc:get_many(["One", "Two", "Two-and-a-half", "Three"]),
    ?_assertEqual([{"One",<<"A">>},{"Two",<<"B">>},{"Two-and-a-half",<<>>},{"Three",<<"C">>}], GetManyResponse).

stats_t(_Setup) ->
    [?_assertMatch([{{?HOST,?PORT}, [{_,_}|_]}], erlmc:stats()),
     ?_assertMatch([{_,_}|_], erlmc:stats(?HOST, ?PORT))].

add_remove_server_t(_Setup) ->
    [?_assertEqual(true, erlmc:has_server(?HOST, ?PORT)),
     ?_assertEqual(ok, erlmc:remove_server(?HOST, ?PORT)),
     ?_assertEqual(false, erlmc:has_server(?HOST, ?PORT))].

set_and_get_timeout_t(_Setup) ->
    erlmc:set("Hello", <<"World">>),
	[?_assertException(exit, {erlmc, {timeout, _}}, erlmc:get("Hello", 0))].

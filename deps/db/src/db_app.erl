-module(db_app).

-behaviour(application).
%% Application callbacks
-export([start/0, start/2, start_debug/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(db).

start(_StartType, _StartArgs) ->
    lager:start(),
    db_sup:start_link().

start_debug() ->
    start(),
    timer:sleep(infinity).


stop(_State) ->
    ok.

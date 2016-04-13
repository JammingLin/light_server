-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_debug/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(game).

start(_StartType, _StartArgs) ->
    lager:start(),
    lager:info("game server starting..."),
    game_sup:start_link().

stop(_State) ->
    lager:info("game server stopped"),
    ok.

start_debug() ->
    start(),
    timer:sleep(infinity).

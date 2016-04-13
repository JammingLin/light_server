%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 十一月 2015 上午10:14
%%%-------------------------------------------------------------------
-module(db_ets_init).
-author("linzhiwei").
-include("db_ets.hrl").
%% API
-export([init/0]).

init()->
    lager:info("db ets init table finished"),
    ok.


%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 九月 2015 上午11:13
%%%-------------------------------------------------------------------
-module(player_status).
-author("linzhiwei").

-define(EMPTY_TIME, datetime:make_datetime(1900, 1, 1, 0, 0, 0)).

%% API
-export([update_last_logon_time/2,
    update_last_leave_time/2]).

-export([get_last_logon_time/1,
    get_last_leave_time/1]).

update_last_logon_time(PlayerID, LastLogonTime) ->
    update_state(PlayerID, last_logon_time, LastLogonTime).

update_last_leave_time(PlayerID, LastLeaveTime) ->
    update_state(PlayerID, last_leave_time, LastLeaveTime).

update_state(PlayerID, StateField, StateValue) when is_atom(StateField) ->
    OldStatus = get_player_status(PlayerID),
    NewStatus = maps:update(StateField, StateValue, OldStatus),
    db_cache:save(player_status, NewStatus).

get_last_logon_time(PlayerID) ->
    get_state(PlayerID, last_logon_time).

get_last_leave_time(PlayerID) ->
    get_state(PlayerID, last_leave_time).

get_state(PlayerID, StateField) when is_atom(StateField) ->
    Status = get_player_status(PlayerID),
    maps:get(StateField, Status).

get_player_status(PlayerID) ->
    Status = db_cache:get(player_status, PlayerID),
    case Status of
        [] ->
            InitStatus = #{ player_id => PlayerID,
                last_logon_time => ?EMPTY_TIME,
                last_leave_time => ?EMPTY_TIME,
                last_feedback => 0},
            InitStatus;
        _ ->
            Status
    end.
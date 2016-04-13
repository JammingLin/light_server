%%% Copyright (C) 2012 Issuu ApS. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(ddb_util).

-export([parameter_value/2,
         first/1,
         epoch/0, epoch/1,
         to_lower/1, to_upper/1,
         to_integer/1,
         separate/2,
         bin2hexstr/1,
         rfc1123_date/0, rfc1123_date/1]).

-define(GREGORIAN_EPOCH_DIFF, 62167219200).

%% @doc Lookup Key in Parameter-list
%%  Will search for Key, but skip certain dummy-values and assume it
%%  is equivalent to a "not-found"
%% @end
-spec parameter_value(A, [{A, B}]) -> not_found | {value, B}.
parameter_value(Key, Parameters) ->
    case lists:keysearch(Key, 1, Parameters) of
        {value, {_, undefined}} -> not_found;
        {value, {_, [16#7F]}}   -> not_found;
        {value, {_, Value}}     -> {value, Value};
        false -> not_found
    end.

-spec to_lower(string()) -> string().
to_lower(String) ->
    lists:map(fun(C) when C >= $A, C =< $Z -> C + 16#20; (C) -> C end,
              String). % Only lowercase ASCII

-spec to_upper(string()) -> string().
to_upper(String) ->
    lists:map(fun(C) when C >= $a, C =< $z -> C - 16#20; (C) -> C end,
              String). % Only lowercase ASCII

-spec to_integer(integer() | list()) -> integer().
to_integer(Integer) when is_integer(Integer) -> Integer;
to_integer(List) -> list_to_integer(List).

%% @doc Given a list of lookups, pick the first valid lookup
%% @end
-spec first([not_found | {value, A}]) -> not_found | {value, A}.
first([])                   -> not_found;
first([not_found | Next])   -> first(Next);
first([{value, _} = R | _]) -> R.

%% @doc Generate number of seconds since Epoch
%% @end
-spec epoch() -> integer().
epoch() ->
    {MS, S, _US} = os:timestamp(),
    MS * 1000000 + S.

%% @doc Generate the number of seconds since Epoch to `DateTime'
%% @end
-spec epoch(term()) -> integer().
epoch(DateTime) ->
    max(0, calendar:datetime_to_gregorian_seconds(DateTime)
               - ?GREGORIAN_EPOCH_DIFF).

%% @doc Generate a valid RFC1123 date
%% @end
-spec rfc1123_date() -> string().
rfc1123_date() -> httpd_util:rfc1123_date().

-type date() :: {1900..3000, 1..12, 1..31}.
-type time() :: {0..23, 0..59, 0..59}.
-type date_time() :: {date(), time()}.

-spec rfc1123_date(integer() | date_time()) -> string().
rfc1123_date(Epoch) when is_integer(Epoch) ->
    rfc1123_date(calendar:gregorian_seconds_to_datetime(
                   Epoch + ?GREGORIAN_EPOCH_DIFF));
rfc1123_date(DateTime) when is_tuple(DateTime) ->
    httpd_util:rfc1123_date(calendar:universal_time_to_local_time(DateTime)).

-spec separate([E], E) -> [E].
separate([], _Separator) -> [];
separate(List, Separator) ->
    Length = length(List),
    lists:foldl(fun(Elem, {N, Acc}) ->
        case N of
            Length -> lists:reverse([Elem | Acc]);
            _ -> {N + 1, [Separator, Elem | Acc]}

        end
    end, {1, []}, List).

-spec bin2hexstr(binary() | string()) -> string().
bin2hexstr(Binary) when is_binary(Binary) ->
    bin2hexstr(binary_to_list(Binary));
bin2hexstr(List) when is_list(List) ->
    Hex = fun(Base10) ->
        io_lib:format("~2.16.0b", [Base10])
    end,
    lists:flatten(lists:map(Hex, List)).


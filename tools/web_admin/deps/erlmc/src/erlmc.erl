%% Copyright (c) 2009 
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol
%% @doc a binary protocol memcached client
-module(erlmc).

-behaviour(gen_server).

-export([start_link/1,
         start_link/2,
		 add_server/3,
         remove_server/2,
         refresh_server/3,
         has_server/2,
         add_connection/2,
         remove_connection/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% api callbacks
-export([get/1,
         get/2,
         get_many/1,
         add/2,
         add/3,
         add/4,
         set/2,
         set/4,
		 replace/2,
         replace/4,
         delete/1,
         increment/4,
         increment/5,
         decrement/4,
         decrement/5,
		 append/2,
         prepend/2,
         stats/0,
         stats/2,
         flush/0,
         flush/1,
         quit/0,
         restart/0,
		 version/0]).

-include("erlmc.hrl").

-define(TIMEOUT, 5000).
-define(CONNECT_TIMEOUT, 5000).

-record(state, {connect_timeout}).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
start_link(CacheServers) ->
    start_link(CacheServers, ?CONNECT_TIMEOUT).

start_link(CacheServers, ConnectTimeout) when is_list(CacheServers), is_integer(ConnectTimeout) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [CacheServers, ConnectTimeout], []).

add_server(Host, Port, PoolSize) ->
    gen_server:call(?MODULE, {add_server, Host, Port, PoolSize}).

refresh_server(Host, Port, PoolSize) ->
    gen_server:call(?MODULE, {refresh_server, Host, Port, PoolSize}).

remove_server(Host, Port) ->
	gen_server:call(?MODULE, {remove_server, Host, Port}).

has_server(Host, Port) ->
    has_server(Host, Port, ?TIMEOUT).

has_server(Host, Port, Timeout) ->
    gen_server:call(?MODULE, {has_server, Host, Port}, Timeout).

add_connection(Host, Port) ->
    add_connection(Host, Port, ?TIMEOUT).

add_connection(Host, Port, Timeout) ->
	gen_server:call(?MODULE, {add_connection, Host, Port}, Timeout).

remove_connection(Host, Port) ->
    remove_connection(Host, Port, ?TIMEOUT).

remove_connection(Host, Port, Timeout) ->
	gen_server:call(?MODULE, {remove_connection, Host, Port}, Timeout).

get(Key0) ->
    ?MODULE:get(Key0, ?TIMEOUT).

get(Key0, Timeout) ->
	Key = package_key(Key0),
    call(map_key(Key), {get, Key}, Timeout).

%% TODO Timeout
get_many(Keys) ->
	Self = self(),
	Pids = [spawn(fun() -> 
		Res = (catch ?MODULE:get(Key)),
		Self ! {self(), {Key, Res}}
	 end) || Key <- Keys],
	lists:reverse(lists:foldl(
		fun(Pid, Acc) ->
			receive
				{Pid, {Key, Res}} -> [{Key, Res}|Acc]
			after ?TIMEOUT ->
				Acc
			end
		end, [], Pids)).

add(Key, Value) ->
	add(Key, Value, 0).

add(Key, Value, Expiration) ->
    add(Key, Value, Expiration, ?TIMEOUT).

add(Key0, Value, Expiration, Timeout) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    call(map_key(Key), {add, Key, Value, Expiration}, Timeout).

set(Key, Value) ->
	set(Key, Value, 0, ?TIMEOUT).
	
set(Key0, Value, Expiration, Timeout) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    call(map_key(Key), {set, Key, Value, Expiration}, Timeout).
    
replace(Key, Value) ->
	replace(Key, Value, 0, ?TIMEOUT).
	
replace(Key0, Value, Expiration, Timeout) when is_binary(Value), is_integer(Expiration) ->
	Key = package_key(Key0),
    call(map_key(Key), {replace, Key, Value, Expiration}, Timeout).
    
delete(Key0) ->
    delete(Key0, ?TIMEOUT).

delete(Key0, Timeout) ->
	Key = package_key(Key0),
    call(map_key(Key), {delete, Key}, Timeout).

increment(Key0, Value, Initial, Expiration) ->
    increment(Key0, Value, Initial, Expiration, ?TIMEOUT).

increment(Key0, Value, Initial, Expiration, Timeout)
  when is_binary(Value), is_binary(Initial), is_integer(Expiration), is_integer(Timeout) ->
	Key = package_key(Key0),
    call(map_key(Key), {increment, Key, Value, Initial, Expiration}, Timeout).

decrement(Key0, Value, Initial, Expiration) ->
    decrement(Key0, Value, Initial, Expiration, ?TIMEOUT).

decrement(Key0, Value, Initial, Expiration, Timeout) when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
	Key = package_key(Key0),
    call(map_key(Key), {decrement, Key, Value, Initial, Expiration}, Timeout).

append(Key0, Value) ->
    append(Key0, Value, ?TIMEOUT).

append(Key0, Value, Timeout) when is_binary(Value) ->
	Key = package_key(Key0),
    call(map_key(Key), {append, Key, Value}, Timeout).

prepend(Key0, Value) ->
    prepend(Key0, Value, ?TIMEOUT).

prepend(Key0, Value, Timeout) when is_binary(Value) ->
	Key = package_key(Key0),
    call(map_key(Key), {prepend, Key, Value}, Timeout).

stats() ->
	multi_call(stats).

stats(Host, Port) ->
    host_port_call(Host, Port, stats).

flush() ->
    multi_call(flush).
    
flush(Expiration) when is_integer(Expiration) ->
    multi_call({flush, Expiration}).
    

quit() ->
    quit(?TIMEOUT).

quit(Timeout) ->
	[begin
		{Key, [
			{'EXIT',{shutdown,{gen_server,call,[Pid,quit,Timeout]}}} ==
				(catch gen_server:call(Pid, quit, Timeout)) || Pid <- Pids]}
	 end || {Key, Pids} <- unique_connections()].

%% Quit forcefully and restart from a clean slate
restart() ->
    ok = gen_server:call(?MODULE, restart).

version() ->
    multi_call(version).

multi_call(Msg) ->
    multi_call(Msg, ?TIMEOUT).

multi_call(Msg, Timeout) ->
	[begin
		Pid = lists:nth(random:uniform(length(Pids)), Pids),
		{{Host, Port}, gen_server:call(Pid, Msg, Timeout)}
	end || {{Host, Port}, Pids} <- unique_connections()].

host_port_call(Host, Port, Msg) ->
    Pid = unique_connection(Host, Port),
    gen_server:call(Pid, Msg, ?TIMEOUT).

call(Pid, Msg, Timeout) ->
	try gen_server:call(Pid, Msg, Timeout) of
		{error, Error} -> exit({erlmc, Error});
		Resp -> Resp
    catch exit:{timeout, Reason} ->
        exit({erlmc, {timeout, Reason}})
	end.

%%--------------------------------------------------------------------
%%% Stateful loop
%%--------------------------------------------------------------------	

%% TODO Turn this into a gen_server. No need to reinvent the wheel here.
init([CacheServers, ConnectTimeout]) ->
    %% Trap exit?
	process_flag(trap_exit, true),
	setup_ets(),

    %% Continuum = [{uint(), {Host, Port}}]
	[add_server_to_continuum(Host, Port) || {Host, Port, _} <- CacheServers],

    %% Connections = [{{Host,Port}, ConnPid}]
	[begin
		[start_connection(Host, Port, ConnectTimeout) || _ <- lists:seq(1, ConnPoolSize)]
	 end || {Host, Port, ConnPoolSize} <- CacheServers],

    {ok, #state{connect_timeout=ConnectTimeout}}.

handle_call({add_server, Host, Port, ConnPoolSize}, _From, #state{connect_timeout=ConnectTimeout}=State) ->
    add_server_to_continuum(Host, Port),
    [start_connection(Host, Port, ConnectTimeout) || _ <- lists:seq(1, ConnPoolSize)],
    {reply, ok, State};

handle_call({refresh_server, Host, Port, ConnPoolSize}, _From, #state{connect_timeout=ConnectTimeout}=State) ->
    % adding to continuum is idempotent
    add_server_to_continuum(Host, Port),
    % add only necessary connections to reach pool size
    LiveConnections = revalidate_connections(Host, Port),
    Reply = if
        LiveConnections < ConnPoolSize ->
            [start_connection(Host, Port, ConnectTimeout) || _ <- lists:seq(1, ConnPoolSize - LiveConnections)],
            ok;
        true -> ok
    end,
    {reply, Reply, State};

handle_call(restart, _From, State) ->
    [[exit(Pid, kill) || Pid <- Pids] || {_Key, Pids} <- unique_connections()],
    setup_ets(),
    {reply, ok, State};

handle_call({remove_server, Host, Port}, _From, State) ->
    [(catch gen_server:call(Pid, quit, ?TIMEOUT)) || [Pid] <- ets:match(erlmc_connections, {{Host, Port}, '$1'})],
	remove_server_from_continuum(Host, Port),
    {reply, ok, State};

handle_call({has_server, Host, Port}, _From, State) ->
    Reply = is_server_in_continuum(Host, Port),
    {reply, Reply, State};

handle_call({add_connection, Host, Port}, _From, #state{connect_timeout=ConnectTimeout}=State) ->
    start_connection(Host, Port, ConnectTimeout),
    {reply, ok, State};

handle_call({remove_connection, Host, Port}, _From, State) ->
    [[Pid]|_] = ets:match(erlmc_connections, {{Host, Port}, '$1'}),
    %% Fork this one out
    Reply = (catch gen_server:call(Pid, quit, ?TIMEOUT)),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Err}, #state{connect_timeout=ConnectTimeout}=State) ->
    case ets:match(erlmc_connections, {'$1', Pid}) of
    	[[{Host, Port}]] ->
    		ets:delete_object(erlmc_connections, {{Host, Port}, Pid}),
    		case Err of
    			shutdown -> ok;
    			_ -> start_connection(Host, Port, ConnectTimeout)
    		end;
    	_ ->
    		ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_connection(Host, Port, ConnectTimeout) ->
	case erlmc_conn:start_link([Host, Port], ConnectTimeout) of
		{ok, Pid} ->
            true = ets:insert(erlmc_connections, {{Host, Port}, Pid}),
            ok;
		_ -> ok
	end.

revalidate_connections(Host, Port) ->
    [(catch gen_server:call(Pid, version, ?TIMEOUT)) || [Pid] <- ets:match(erlmc_connections, {{Host, Port}, '$1'})],
    length(ets:match(erlmc_connections, {{Host, Port}, '$1'})).

setup_ets() ->
    %% Clear out any pre-existing data so we can start clean
    [case ets:info(T) of
         undefined -> ok;
         _ -> ets:delete(T)
     end || T <- [erlmc_connections, erlmc_continuum]],
    ets:new(erlmc_continuum, [ordered_set, protected, named_table]),
    ets:new(erlmc_connections, [bag, protected, named_table]).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_server_to_continuum(Host, Port) ->
	[ets:insert(erlmc_continuum, {hash_to_uint(Host ++ integer_to_list(Port) ++ integer_to_list(I)), {Host, Port}}) || I <- lists:seq(1, 100)].

remove_server_from_continuum(Host, Port) ->
	case ets:match(erlmc_continuum, {'$1', {Host, Port}}) of
		[] -> 
			ok;
		List ->
			[ets:delete(erlmc_continuum, Key) || [Key] <- List]
	end.

is_server_in_continuum(Host, Port) ->
    case ets:match(erlmc_continuum, {'$1', {Host, Port}}) of
        [] -> 
            false;
        _ ->
            true
    end.

package_key(Key) when is_atom(Key) ->
    atom_to_list(Key);

package_key(Key) when is_list(Key) ->
    Key;

package_key(Key) when is_binary(Key) ->
    binary_to_list(Key);

package_key(Key) ->
    lists:flatten(io_lib:format("~p", [Key])).

unique_connections() ->
	dict:to_list(lists:foldl(
		fun({Key, Val}, Dict) ->
			dict:append_list(Key, [Val], Dict)
		end, dict:new(), ets:tab2list(erlmc_connections))).

unique_connection(Host, Port) ->
    case ets:lookup(erlmc_connections, {Host, Port}) of
        [] -> exit({erlmc, {connection_not_found, {Host, Port}}});
        Pids ->
            {_, Pid} = lists:nth(random:uniform(length(Pids)), Pids),
            Pid
    end.

%% Consistent hashing functions
%%
%% First, hash memcached servers to unsigned integers on a continuum. To
%% map a key to a memcached server, hash the key to an unsigned integer
%% and locate the next largest integer on the continuum. That integer
%% represents the hashed server that the key maps to.
%% reference: http://www8.org/w8-papers/2a-webserver/caching/paper2.html
hash_to_uint(Key) when is_list(Key) ->
    <<Int:128/unsigned-integer>> = erlang:md5(Key), Int.

%% @spec map_key(Key) -> Conn
%%		 Key = string()
%%		 Conn = pid()
map_key(Key) when is_list(Key) ->
	First = ets:first(erlmc_continuum),
    {Host, Port} =
		case find_next_largest(hash_to_uint(Key), First) of
			undefined ->
				case First of
					'$end_of_table' -> exit(erlmc_continuum_empty);
					_ ->
						[{_, Value}] = ets:lookup(erlmc_continuum, First),
						Value
				end;
			Value -> Value
		end,
	unique_connection(Host, Port).

%% @todo: use sorting algorithm to find next largest
find_next_largest(_, '$end_of_table') -> 
	undefined;

find_next_largest(Int, Key) when Key > Int ->
	[{_, Val}] = ets:lookup(erlmc_continuum, Key),
	Val;

find_next_largest(Int, Key) ->
	find_next_largest(Int, ets:next(erlmc_continuum, Key)).

%%%-------------------------------------------------------------------
%%% @author long
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 八月 2014 10:38
%%%-------------------------------------------------------------------
-module(m_rand).
-author("long").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([init_seed/0, seed/0, seed/3, random/0, random/1, random/2]).

%% 因为随机数的算法是每一次随机后, 都会生成新的种子, 然后放到进程字典中, 如果是多进程, 只初始化一次随机种子, 是不够的
%% 会造成还原随机数的困难

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init_seed() ->
    <<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
    seed(A, B, C).

%% Seeds random number generation with default (fixed) values in the process dictionary,
%% and returns the old state.
seed() ->
    gen_server:call(?SERVER, seed).

%% Seeds random number generation with integer values in the process dictionary, and returns the old state.
%% One way of obtaining a seed is to use the BIF now/0:
seed(A1, A2, A3) ->
    gen_server:call(?SERVER, {seed, A1, A2, A3}).

%% Returns a random float uniformly distributed between 0.0 and 1.0,
%% updating the state in the process dictionary.
random() ->
    gen_server:call(?SERVER, random).

%% Given an integer N >= 1, uniform/1 returns a random integer uniformly distributed between 1 and N,
%% updating the state in the process dictionary.
random(N) when is_integer(N), N >= 1 ->
    gen_server:call(?SERVER, {random, N}).

%% 在指定的范围内随机, 返回值大于等于Min, 小于等于Max
random(Min, Max) when is_integer(Min), is_integer(Max), Min >= 1, Max > Min ->
    gen_server:call(?SERVER, {random, Min, Max}).

init([]) ->
    {ok, []}.

handle_call(seed, _From, State) ->
    {reply, random:seed(), State};
handle_call({seed, A1, A2, A3}, _From, State) ->
    {reply, random:seed(A1, A2, A3), State};
handle_call(random, _From, State) ->
    {reply, random:uniform(), State};
handle_call({random, N}, _From, State) ->
    {reply, random:uniform(N), State};
handle_call({random, Min, Max}, _From, State) ->
    N = Max - Min + 1,
    Rand = Min-1 + random:uniform(N),
    {reply, Rand, State}.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

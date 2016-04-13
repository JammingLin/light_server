-module(aleppo_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/home/jie/erlang/18.1/lib/parsetools-2.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/aleppo_parser.erl", 178).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_2(2, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, comment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, eof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 'yeccgoto_\'File\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_3_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_4_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_5_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_6_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_7_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2_170(181, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_9_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_170(170, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_11_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_48/7}).
yeccpars2_48(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 'yeccgoto_\'File\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_!'(Stack),
 yeccpars2_97(97, '!', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_#'(Stack),
 yeccpars2_97(97, '#', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_('(Stack),
 yeccpars2_97(97, '(', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_*'(Stack),
 yeccpars2_97(97, '*', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_+'(Stack),
 yeccpars2_97(97, '+', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_++'(Stack),
 yeccpars2_97(97, '++', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_-'(Stack),
 yeccpars2_97(97, '-', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_--'(Stack),
 yeccpars2_97(97, '--', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_->'(Stack),
 yeccpars2_97(97, '->', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_.'(Stack),
 yeccpars2_97(97, '.', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_..'(Stack),
 yeccpars2_97(97, '..', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_...'(Stack),
 yeccpars2_97(97, '...', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_/'(Stack),
 yeccpars2_97(97, '/', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_/='(Stack),
 yeccpars2_97(97, '/=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_:'(Stack),
 yeccpars2_97(97, ':', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_:-'(Stack),
 yeccpars2_97(97, ':-', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_::'(Stack),
 yeccpars2_97(97, '::', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_:='(Stack),
 yeccpars2_97(97, ':=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_;'(Stack),
 yeccpars2_97(97, ';', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_<'(Stack),
 yeccpars2_97(97, '<', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_<-'(Stack),
 yeccpars2_97(97, '<-', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_<<'(Stack),
 yeccpars2_97(97, '<<', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_<='(Stack),
 yeccpars2_97(97, '<=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_='(Stack),
 yeccpars2_97(97, '=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_=/='(Stack),
 yeccpars2_97(97, '=/=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_=:='(Stack),
 yeccpars2_97(97, '=:=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_=<'(Stack),
 yeccpars2_97(97, '=<', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_=='(Stack),
 yeccpars2_97(97, '==', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_=>'(Stack),
 yeccpars2_97(97, '=>', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_>'(Stack),
 yeccpars2_97(97, '>', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_>='(Stack),
 yeccpars2_97(97, '>=', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_>>'(Stack),
 yeccpars2_97(97, '>>', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_?'(Stack),
 yeccpars2_97(97, '?', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_['(Stack),
 yeccpars2_97(97, '[', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_after(Stack),
 yeccpars2_97(97, 'after', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_and(Stack),
 yeccpars2_97(97, 'and', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_andalso(Stack),
 yeccpars2_97(97, 'andalso', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_atom(Stack),
 yeccpars2_97(97, atom, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_band(Stack),
 yeccpars2_97(97, 'band', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_begin(Stack),
 yeccpars2_97(97, 'begin', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_bnot(Stack),
 yeccpars2_97(97, 'bnot', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_bor(Stack),
 yeccpars2_97(97, 'bor', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_bsl(Stack),
 yeccpars2_97(97, 'bsl', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_bsr(Stack),
 yeccpars2_97(97, 'bsr', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_bxor(Stack),
 yeccpars2_97(97, 'bxor', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_case(Stack),
 yeccpars2_97(97, 'case', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_catch(Stack),
 yeccpars2_97(97, 'catch', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_char(Stack),
 yeccpars2_97(97, char, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_comment(Stack),
 yeccpars2_97(97, comment, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_div(Stack),
 yeccpars2_97(97, 'div', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_end(Stack),
 yeccpars2_97(97, 'end', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_float(Stack),
 yeccpars2_97(97, float, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_fun(Stack),
 yeccpars2_97(97, 'fun', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_if(Stack),
 yeccpars2_97(97, 'if', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_integer(Stack),
 yeccpars2_97(97, integer, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_not(Stack),
 yeccpars2_97(97, 'not', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_of(Stack),
 yeccpars2_97(97, 'of', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_or(Stack),
 yeccpars2_97(97, 'or', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_orelse(Stack),
 yeccpars2_97(97, 'orelse', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, query, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_query(Stack),
 yeccpars2_97(97, query, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_receive(Stack),
 yeccpars2_97(97, 'receive', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_rem(Stack),
 yeccpars2_97(97, 'rem', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_spec(Stack),
 yeccpars2_97(97, spec, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_string(Stack),
 yeccpars2_97(97, string, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_try(Stack),
 yeccpars2_97(97, 'try', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_var(Stack),
 yeccpars2_97(97, var, [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_when(Stack),
 yeccpars2_97(97, 'when', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_xor(Stack),
 yeccpars2_97(97, 'xor', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_{'(Stack),
 yeccpars2_97(97, '{', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_|'(Stack),
 yeccpars2_97(97, '|', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_94_||'(Stack),
 yeccpars2_97(97, '||', [94 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccpars2_98(98, Cat, [94 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_95(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, query, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), query, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ApplyMacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_97(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_cont_97(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, comment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, query, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_98/7}).
yeccpars2_98(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_!'(Stack),
 yeccpars2_97(97, '!', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_#'(Stack),
 yeccpars2_97(97, '#', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_('(Stack),
 yeccpars2_97(97, '(', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_*'(Stack),
 yeccpars2_97(97, '*', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_+'(Stack),
 yeccpars2_97(97, '+', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_++'(Stack),
 yeccpars2_97(97, '++', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_-'(Stack),
 yeccpars2_97(97, '-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_--'(Stack),
 yeccpars2_97(97, '--', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_->'(Stack),
 yeccpars2_97(97, '->', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_.'(Stack),
 yeccpars2_97(97, '.', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_..'(Stack),
 yeccpars2_97(97, '..', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_...'(Stack),
 yeccpars2_97(97, '...', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_/'(Stack),
 yeccpars2_97(97, '/', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_/='(Stack),
 yeccpars2_97(97, '/=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_:'(Stack),
 yeccpars2_97(97, ':', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_:-'(Stack),
 yeccpars2_97(97, ':-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_::'(Stack),
 yeccpars2_97(97, '::', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_:='(Stack),
 yeccpars2_97(97, ':=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_;'(Stack),
 yeccpars2_97(97, ';', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<'(Stack),
 yeccpars2_97(97, '<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<-'(Stack),
 yeccpars2_97(97, '<-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<<'(Stack),
 yeccpars2_97(97, '<<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<='(Stack),
 yeccpars2_97(97, '<=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_='(Stack),
 yeccpars2_97(97, '=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=/='(Stack),
 yeccpars2_97(97, '=/=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=:='(Stack),
 yeccpars2_97(97, '=:=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=<'(Stack),
 yeccpars2_97(97, '=<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=='(Stack),
 yeccpars2_97(97, '==', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=>'(Stack),
 yeccpars2_97(97, '=>', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>'(Stack),
 yeccpars2_97(97, '>', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>='(Stack),
 yeccpars2_97(97, '>=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>>'(Stack),
 yeccpars2_97(97, '>>', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_?'(Stack),
 yeccpars2_97(97, '?', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_['(Stack),
 yeccpars2_97(97, '[', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_after(Stack),
 yeccpars2_97(97, 'after', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_and(Stack),
 yeccpars2_97(97, 'and', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_andalso(Stack),
 yeccpars2_97(97, 'andalso', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_atom(Stack),
 yeccpars2_97(97, atom, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_band(Stack),
 yeccpars2_97(97, 'band', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_begin(Stack),
 yeccpars2_97(97, 'begin', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bnot(Stack),
 yeccpars2_97(97, 'bnot', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bor(Stack),
 yeccpars2_97(97, 'bor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bsl(Stack),
 yeccpars2_97(97, 'bsl', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bsr(Stack),
 yeccpars2_97(97, 'bsr', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bxor(Stack),
 yeccpars2_97(97, 'bxor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_case(Stack),
 yeccpars2_97(97, 'case', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_catch(Stack),
 yeccpars2_97(97, 'catch', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_char(Stack),
 yeccpars2_97(97, char, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_comment(Stack),
 yeccpars2_97(97, comment, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_div(Stack),
 yeccpars2_97(97, 'div', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_end(Stack),
 yeccpars2_97(97, 'end', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_float(Stack),
 yeccpars2_97(97, float, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_fun(Stack),
 yeccpars2_97(97, 'fun', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_if(Stack),
 yeccpars2_97(97, 'if', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_integer(Stack),
 yeccpars2_97(97, integer, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_not(Stack),
 yeccpars2_97(97, 'not', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_of(Stack),
 yeccpars2_97(97, 'of', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_or(Stack),
 yeccpars2_97(97, 'or', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_orelse(Stack),
 yeccpars2_97(97, 'orelse', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, query, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_query(Stack),
 yeccpars2_97(97, query, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_receive(Stack),
 yeccpars2_97(97, 'receive', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_rem(Stack),
 yeccpars2_97(97, 'rem', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_spec(Stack),
 yeccpars2_97(97, spec, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_string(Stack),
 yeccpars2_97(97, string, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_try(Stack),
 yeccpars2_97(97, 'try', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_var(Stack),
 yeccpars2_97(97, var, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_when(Stack),
 yeccpars2_97(97, 'when', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_xor(Stack),
 yeccpars2_97(97, 'xor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_{'(Stack),
 yeccpars2_97(97, '{', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_|'(Stack),
 yeccpars2_97(97, '|', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_||'(Stack),
 yeccpars2_97(97, '||', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccpars2_118(118, Cat, [103 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_!'(Stack),
 yeccpars2_97(97, '!', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_#'(Stack),
 yeccpars2_97(97, '#', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_('(Stack),
 yeccpars2_97(97, '(', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_*'(Stack),
 yeccpars2_97(97, '*', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_+'(Stack),
 yeccpars2_97(97, '+', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_++'(Stack),
 yeccpars2_97(97, '++', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_-'(Stack),
 yeccpars2_97(97, '-', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_--'(Stack),
 yeccpars2_97(97, '--', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_->'(Stack),
 yeccpars2_97(97, '->', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_.'(Stack),
 yeccpars2_97(97, '.', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_..'(Stack),
 yeccpars2_97(97, '..', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_...'(Stack),
 yeccpars2_97(97, '...', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_/'(Stack),
 yeccpars2_97(97, '/', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_/='(Stack),
 yeccpars2_97(97, '/=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_:'(Stack),
 yeccpars2_97(97, ':', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_:-'(Stack),
 yeccpars2_97(97, ':-', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_::'(Stack),
 yeccpars2_97(97, '::', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_:='(Stack),
 yeccpars2_97(97, ':=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_;'(Stack),
 yeccpars2_97(97, ';', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_<'(Stack),
 yeccpars2_97(97, '<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_<-'(Stack),
 yeccpars2_97(97, '<-', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_<<'(Stack),
 yeccpars2_97(97, '<<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_<='(Stack),
 yeccpars2_97(97, '<=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_='(Stack),
 yeccpars2_97(97, '=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_=/='(Stack),
 yeccpars2_97(97, '=/=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_=:='(Stack),
 yeccpars2_97(97, '=:=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_=<'(Stack),
 yeccpars2_97(97, '=<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_=='(Stack),
 yeccpars2_97(97, '==', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_=>'(Stack),
 yeccpars2_97(97, '=>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_>'(Stack),
 yeccpars2_97(97, '>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_>='(Stack),
 yeccpars2_97(97, '>=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_>>'(Stack),
 yeccpars2_97(97, '>>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_?'(Stack),
 yeccpars2_97(97, '?', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_['(Stack),
 yeccpars2_97(97, '[', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_after(Stack),
 yeccpars2_97(97, 'after', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_and(Stack),
 yeccpars2_97(97, 'and', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_andalso(Stack),
 yeccpars2_97(97, 'andalso', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_atom(Stack),
 yeccpars2_97(97, atom, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_band(Stack),
 yeccpars2_97(97, 'band', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_begin(Stack),
 yeccpars2_97(97, 'begin', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_bnot(Stack),
 yeccpars2_97(97, 'bnot', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_bor(Stack),
 yeccpars2_97(97, 'bor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_bsl(Stack),
 yeccpars2_97(97, 'bsl', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_bsr(Stack),
 yeccpars2_97(97, 'bsr', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_bxor(Stack),
 yeccpars2_97(97, 'bxor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_case(Stack),
 yeccpars2_97(97, 'case', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_catch(Stack),
 yeccpars2_97(97, 'catch', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_char(Stack),
 yeccpars2_97(97, char, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_comment(Stack),
 yeccpars2_97(97, comment, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_div(Stack),
 yeccpars2_97(97, 'div', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_end(Stack),
 yeccpars2_97(97, 'end', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_float(Stack),
 yeccpars2_97(97, float, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_fun(Stack),
 yeccpars2_97(97, 'fun', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_if(Stack),
 yeccpars2_97(97, 'if', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_integer(Stack),
 yeccpars2_97(97, integer, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_not(Stack),
 yeccpars2_97(97, 'not', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_of(Stack),
 yeccpars2_97(97, 'of', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_or(Stack),
 yeccpars2_97(97, 'or', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_orelse(Stack),
 yeccpars2_97(97, 'orelse', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, query, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_query(Stack),
 yeccpars2_97(97, query, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_receive(Stack),
 yeccpars2_97(97, 'receive', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_rem(Stack),
 yeccpars2_97(97, 'rem', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_spec(Stack),
 yeccpars2_97(97, spec, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_string(Stack),
 yeccpars2_97(97, string, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_try(Stack),
 yeccpars2_97(97, 'try', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_var(Stack),
 yeccpars2_97(97, var, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_when(Stack),
 yeccpars2_97(97, 'when', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_xor(Stack),
 yeccpars2_97(97, 'xor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_{'(Stack),
 yeccpars2_97(97, '{', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_|'(Stack),
 yeccpars2_97(97, '|', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_||'(Stack),
 yeccpars2_97(97, '||', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccpars2_114(114, Cat, [106 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_107(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_!'(Stack),
 yeccpars2_97(97, '!', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_#'(Stack),
 yeccpars2_97(97, '#', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_('(Stack),
 yeccpars2_97(97, '(', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_*'(Stack),
 yeccpars2_97(97, '*', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_+'(Stack),
 yeccpars2_97(97, '+', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_++'(Stack),
 yeccpars2_97(97, '++', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_-'(Stack),
 yeccpars2_97(97, '-', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_--'(Stack),
 yeccpars2_97(97, '--', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_->'(Stack),
 yeccpars2_97(97, '->', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_.'(Stack),
 yeccpars2_97(97, '.', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_..'(Stack),
 yeccpars2_97(97, '..', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_...'(Stack),
 yeccpars2_97(97, '...', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_/'(Stack),
 yeccpars2_97(97, '/', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_/='(Stack),
 yeccpars2_97(97, '/=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_:'(Stack),
 yeccpars2_97(97, ':', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_:-'(Stack),
 yeccpars2_97(97, ':-', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_::'(Stack),
 yeccpars2_97(97, '::', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_:='(Stack),
 yeccpars2_97(97, ':=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_;'(Stack),
 yeccpars2_97(97, ';', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_<'(Stack),
 yeccpars2_97(97, '<', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_<-'(Stack),
 yeccpars2_97(97, '<-', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_<<'(Stack),
 yeccpars2_97(97, '<<', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_<='(Stack),
 yeccpars2_97(97, '<=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_='(Stack),
 yeccpars2_97(97, '=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_=/='(Stack),
 yeccpars2_97(97, '=/=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_=:='(Stack),
 yeccpars2_97(97, '=:=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_=<'(Stack),
 yeccpars2_97(97, '=<', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_=='(Stack),
 yeccpars2_97(97, '==', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_=>'(Stack),
 yeccpars2_97(97, '=>', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_>'(Stack),
 yeccpars2_97(97, '>', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_>='(Stack),
 yeccpars2_97(97, '>=', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_>>'(Stack),
 yeccpars2_97(97, '>>', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_?'(Stack),
 yeccpars2_97(97, '?', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_['(Stack),
 yeccpars2_97(97, '[', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_after(Stack),
 yeccpars2_97(97, 'after', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_and(Stack),
 yeccpars2_97(97, 'and', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_andalso(Stack),
 yeccpars2_97(97, 'andalso', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_atom(Stack),
 yeccpars2_97(97, atom, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_band(Stack),
 yeccpars2_97(97, 'band', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_begin(Stack),
 yeccpars2_97(97, 'begin', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_bnot(Stack),
 yeccpars2_97(97, 'bnot', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_bor(Stack),
 yeccpars2_97(97, 'bor', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_bsl(Stack),
 yeccpars2_97(97, 'bsl', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_bsr(Stack),
 yeccpars2_97(97, 'bsr', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_bxor(Stack),
 yeccpars2_97(97, 'bxor', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_case(Stack),
 yeccpars2_97(97, 'case', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_catch(Stack),
 yeccpars2_97(97, 'catch', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_char(Stack),
 yeccpars2_97(97, char, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_comment(Stack),
 yeccpars2_97(97, comment, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_div(Stack),
 yeccpars2_97(97, 'div', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_end(Stack),
 yeccpars2_97(97, 'end', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_float(Stack),
 yeccpars2_97(97, float, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_fun(Stack),
 yeccpars2_97(97, 'fun', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_if(Stack),
 yeccpars2_97(97, 'if', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_integer(Stack),
 yeccpars2_97(97, integer, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_not(Stack),
 yeccpars2_97(97, 'not', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_of(Stack),
 yeccpars2_97(97, 'of', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_or(Stack),
 yeccpars2_97(97, 'or', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_orelse(Stack),
 yeccpars2_97(97, 'orelse', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, query, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_query(Stack),
 yeccpars2_97(97, query, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_receive(Stack),
 yeccpars2_97(97, 'receive', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_rem(Stack),
 yeccpars2_97(97, 'rem', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_spec(Stack),
 yeccpars2_97(97, spec, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_string(Stack),
 yeccpars2_97(97, string, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_try(Stack),
 yeccpars2_97(97, 'try', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_var(Stack),
 yeccpars2_97(97, var, [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_when(Stack),
 yeccpars2_97(97, 'when', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_xor(Stack),
 yeccpars2_97(97, 'xor', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_{'(Stack),
 yeccpars2_97(97, '{', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_|'(Stack),
 yeccpars2_97(97, '|', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_107_||'(Stack),
 yeccpars2_97(97, '||', [107 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2_110(110, Cat, [107 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_108(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, query, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), query, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
yeccpars2_110(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccpars2_97(97, Cat, [112 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_113(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, query, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), query, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_114/7}).
yeccpars2_114(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
yeccpars2_116(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 'yeccgoto_\'MacroString\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_118/7}).
yeccpars2_118(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 yeccpars2_97(97, Cat, [120 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_121(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, query, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), query, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
yeccpars2_122(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_123/7}).
yeccpars2_123(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_124/7}).
yeccpars2_124(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_125/7}).
yeccpars2_125(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_126/7}).
yeccpars2_126(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_127/7}).
yeccpars2_127(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_128: see yeccpars2_48

-dialyzer({nowarn_function, yeccpars2_129/7}).
yeccpars2_129(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_130/7}).
yeccpars2_130(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 'yeccgoto_\'Undef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_132/7}).
yeccpars2_132(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_133/7}).
yeccpars2_133(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_134/7}).
yeccpars2_134(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 'yeccgoto_\'IncludeLib\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_136/7}).
yeccpars2_136(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_137/7}).
yeccpars2_137(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_138/7}).
yeccpars2_138(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 'yeccgoto_\'Include\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_140: see yeccpars2_48

-dialyzer({nowarn_function, yeccpars2_141/7}).
yeccpars2_141(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_142/7}).
yeccpars2_142(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 'yeccgoto_\'IfNDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_144: see yeccpars2_48

-dialyzer({nowarn_function, yeccpars2_145/7}).
yeccpars2_145(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_146/7}).
yeccpars2_146(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 'yeccgoto_\'IfDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_148: see yeccpars2_48

-dialyzer({nowarn_function, yeccpars2_149/7}).
yeccpars2_149(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_150(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccpars2_161(161, Cat, [150 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
yeccpars2_151(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccpars2_153(153, Cat, [152 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_153(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_156_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_157(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_160(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
yeccpars2_161(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_163/7}).
yeccpars2_163(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 yeccpars2_165(165, Cat, [164 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_165(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_168/7}).
yeccpars2_168(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_169_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_170(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccpars2_178(178, Cat, [172 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_173(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_174/7}).
yeccpars2_174(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_175/7}).
yeccpars2_175(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 'yeccgoto_\'EndIf\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_177_(Stack),
 'yeccgoto_\'Else\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_178(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_179_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_180(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_181: see yeccpars2_170

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccpars2_178(184, Cat, [183 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_184: see yeccpars2_178

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'ApplyMacroArgs\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(98, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Define\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Elements\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(181, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(170, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(184, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Else\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Else\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'EndIf\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Expression\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionList\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionToken\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'File\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'FormTokens\''(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FormTokens\''(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Include\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IncludeLib\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Macro\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroArgs\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroName\''(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(141, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroString\''(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyApplyMacroArgs\''(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpression\''(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpressionList\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyMacroArgs\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Token\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Undef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(170=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/aleppo_parser.yrl", 56).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_2_/1}).
-file("src/aleppo_parser.yrl", 54).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ { eof , 0 } ]
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-file("src/aleppo_parser.yrl", 59).
yeccpars2_3_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("src/aleppo_parser.yrl", 64).
yeccpars2_4_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("src/aleppo_parser.yrl", 60).
yeccpars2_5_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("src/aleppo_parser.yrl", 63).
yeccpars2_6_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/aleppo_parser.yrl", 62).
yeccpars2_7_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("src/aleppo_parser.yrl", 56).
yeccpars2_8_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_9_/1}).
-file("src/aleppo_parser.yrl", 58).
yeccpars2_9_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/aleppo_parser.yrl", 56).
yeccpars2_10_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("src/aleppo_parser.yrl", 57).
yeccpars2_11_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/aleppo_parser.yrl", 61).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("src/aleppo_parser.yrl", 65).
yeccpars2_67_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/aleppo_parser.yrl", 53).
yeccpars2_69_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("src/aleppo_parser.yrl", 92).
yeccpars2_91_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_94_!'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_#'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_('/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_*'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_+'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_++'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_--'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_->'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_.'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_..'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_..'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_...'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_...'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_/'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_:'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_:-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_::'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_;'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_<-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_<<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_<='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_=/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_=:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_=<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_=='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_=>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_=>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_>='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_>>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_?'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_['/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_after/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_and/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_andalso/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_atom/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_band/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_begin/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_bnot/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_bor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_bsl/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_bsr/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_bxor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_case/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_catch/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_char/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_comment/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_div/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_end/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_float/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_fun/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_if/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_integer/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_not/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_of/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_or/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_orelse/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_query/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_receive/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_rem/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_spec/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_string/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_try/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_var/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_when/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_xor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_94_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_{'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_|'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_94_||'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_94_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_/1}).
-file("src/aleppo_parser.yrl", 106).
yeccpars2_94_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_95_/1}).
-file("src/aleppo_parser.yrl", 109).
yeccpars2_95_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/aleppo_parser.yrl", 93).
yeccpars2_99_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/aleppo_parser.yrl", 114).
yeccpars2_100_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("src/aleppo_parser.yrl", 113).
yeccpars2_101_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("src/aleppo_parser.yrl", 112).
yeccpars2_102_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,'yeccpars2_103_!'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_#'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_('/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_*'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_+'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_++'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_--'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_->'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_.'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_..'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_..'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_...'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_...'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_/'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_:'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_:-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_::'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_;'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_=>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_?'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_['/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_after/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_and/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_andalso/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_atom/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_band/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_begin/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bnot/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bsl/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bsr/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bxor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_case/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_catch/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_char/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_comment/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_div/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_end/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_float/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_fun/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_if/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_integer/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_not/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_of/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_or/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_orelse/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_query/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_receive/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_rem/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_spec/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_string/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_try/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_var/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_when/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_xor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_103_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_{'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_|'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_||'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_103_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_/1}).
-file("src/aleppo_parser.yrl", 122).
yeccpars2_103_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_!'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_#'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_('/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_*'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_+'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_++'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_--'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_->'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_.'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_..'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_..'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_...'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_...'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_/'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_:'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_:-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_::'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_;'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_<-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_<<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_<='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_=/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_=:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_=<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_=='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_=>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_=>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_>='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_>>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_?'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_['/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_after/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_and/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_andalso/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_atom/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_band/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_begin/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_bnot/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_bor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_bsl/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_bsr/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_bxor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_case/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_catch/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_char/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_comment/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_div/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_end/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_float/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_fun/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_if/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_integer/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_not/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_of/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_or/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_orelse/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_query/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_receive/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_rem/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_spec/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_string/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_try/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_var/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_when/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_xor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_106_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_{'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_|'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_||'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_106_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_/1}).
-file("src/aleppo_parser.yrl", 122).
yeccpars2_106_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_!'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_#'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_('/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_*'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_+'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_++'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_--'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_->'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_.'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_..'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_..'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_...'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_...'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_/'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_:'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_:-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_::'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_;'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_<-'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_<<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_<='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_=/='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_=:='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_=<'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_=='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_=>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_=>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_>='/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_>>'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_?'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_['/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_after/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_and/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_andalso/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_atom/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_band/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_begin/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_bnot/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_bor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_bsl/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_bsr/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_bxor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_case/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_catch/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_char/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_comment/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_div/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_end/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_float/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_fun/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_if/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_integer/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_not/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_of/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_or/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_orelse/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_query/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_receive/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_rem/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_spec/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_string/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_try/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_var/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_when/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_xor/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_107_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_{'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_|'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_107_||'/1}).
-file("src/aleppo_parser.yrl", 119).
'yeccpars2_107_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_/1}).
-file("src/aleppo_parser.yrl", 122).
yeccpars2_107_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_111_/1}).
-file("src/aleppo_parser.yrl", 116).
yeccpars2_111_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_112_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_113_/1}).
-file("src/aleppo_parser.yrl", 125).
yeccpars2_113_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("src/aleppo_parser.yrl", 117).
yeccpars2_115_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-file("src/aleppo_parser.yrl", 95).
yeccpars2_117_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_string , __3 }
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("src/aleppo_parser.yrl", 115).
yeccpars2_119_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("src/aleppo_parser.yrl", 119).
yeccpars2_120_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_121_/1}).
-file("src/aleppo_parser.yrl", 110).
yeccpars2_121_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("src/aleppo_parser.yrl", 90).
yeccpars2_131_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_undef , __4 }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("src/aleppo_parser.yrl", 77).
yeccpars2_135_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include_lib , __4 }
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("src/aleppo_parser.yrl", 76).
yeccpars2_139_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include , __4 }
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("src/aleppo_parser.yrl", 86).
yeccpars2_143_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-file("src/aleppo_parser.yrl", 85).
yeccpars2_147_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("src/aleppo_parser.yrl", 100).
yeccpars2_150_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_152_/1}).
-file("src/aleppo_parser.yrl", 67).
yeccpars2_152_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_154_/1}).
-file("src/aleppo_parser.yrl", 68).
yeccpars2_154_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("src/aleppo_parser.yrl", 70).
yeccpars2_155_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-file("src/aleppo_parser.yrl", 69).
yeccpars2_156_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("src/aleppo_parser.yrl", 73).
yeccpars2_158_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-file("src/aleppo_parser.yrl", 72).
yeccpars2_159_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 }
  end | __Stack].

-compile({inline,yeccpars2_162_/1}).
-file("src/aleppo_parser.yrl", 104).
yeccpars2_162_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ [ __1 ] ]
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("src/aleppo_parser.yrl", 67).
yeccpars2_164_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_167_/1}).
-file("src/aleppo_parser.yrl", 74).
yeccpars2_167_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 , __9 }
  end | __Stack].

-compile({inline,yeccpars2_169_/1}).
-file("src/aleppo_parser.yrl", 103).
yeccpars2_169_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ [ __3 ] ]
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("src/aleppo_parser.yrl", 80).
yeccpars2_171_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("src/aleppo_parser.yrl", 56).
yeccpars2_172_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_176_/1}).
-file("src/aleppo_parser.yrl", 0).
yeccpars2_176_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-file("src/aleppo_parser.yrl", 0).
yeccpars2_177_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-file("src/aleppo_parser.yrl", 79).
yeccpars2_179_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-file("src/aleppo_parser.yrl", 83).
yeccpars2_182_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_183_/1}).
-file("src/aleppo_parser.yrl", 56).
yeccpars2_183_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_185_/1}).
-file("src/aleppo_parser.yrl", 82).
yeccpars2_185_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 , __4 }
  end | __Stack].



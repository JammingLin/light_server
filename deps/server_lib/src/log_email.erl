%%%-------------------------------------------------------------------
%%% @author linzhiwei
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 三月 2015 上午11:34
%%%-------------------------------------------------------------------
-module(log_email).
-author("linzhiwei").
-include("log_email.hrl").
%% API
-export([send/1]).
-compile(export_all).

-define(MAX_SIZE, 204800000).
-define(DE, io:format("~p:~p~n", [?FILE, ?LINE])).

%% send email by email record
send(Email) when
    undefined =/= Email#email_config.server_ip,
    undefined =/= Email#email_config.account,
    undefined =/= Email#email_config.to_emails,
    undefined =/= Email#email_config.password ->
    ServerPort =
        case Email#email_config.server_port of
            undefined -> case Email#email_config.ssl of
                             true  -> ?SSL_SERV_PORT_DEF;
                             false -> ?NOT_SSL_SERV_PORT_DEF
                         end;
            Any       -> Any
        end,
    Sock =
        case Email#email_config.ssl of
            false -> {ok, Socket} =
                gen_tcp:connect(Email#email_config.server_ip,
                    ServerPort,
                    [binary, {active, false}, {packet, 0}]),
                #socket{type = tcp, sock = Socket};
            true  -> ssl:start(),
                {ok, Socket} =
                    ssl:connect(Email#email_config.server_ip,
                        ServerPort,
                        [binary, {active, false}, {packet, 0}],
                        infinity),
                #socket{type = ssl, sock = Socket}
        end,
    connect_email(Sock, Email),
    send_email_head(Sock, Email),
    send_email_info(Sock, Email),
    send_email_data(Sock, Email),
    end_email(Sock),
    case Sock#socket.type of
        ssl -> ssl:close(Sock#socket.sock),
            ssl:stop();
        tcp -> gen_tcp:close(Sock#socket.sock)
    end.

%% connect your email
connect_email(Sock, Email) ->
    send_socket(Sock, "HELO " ++ Email#email_config.account ++ "\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "AUTH LOGIN\r\n"),
    recv_socket(Sock),

    send_socket(Sock, base64:encode(Email#email_config.account)),
    send_socket(Sock, "\r\n"),
    recv_socket(Sock),

    send_socket(Sock, base64:encode(Email#email_config.password)),
    send_socket(Sock, "\r\n"),
    recv_socket(Sock).

%% send email head
send_email_head(Sock, Email) ->
    send_socket(Sock, "MAIL FROM <" ++ Email#email_config.account ++ ">\r\n"),
    recv_socket(Sock),

    rcpt_to_emails(Sock, Email#email_config.to_emails),
    recv_socket(Sock).

%% send email info
send_email_info(Sock, Email) ->
    send_socket(Sock, "DATA\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "FROM:<" ++ Email#email_config.account ++ ">\r\n"),
    recv_socket(Sock),

    Subject = unicode:characters_to_list(Email#email_config.subject),
    send_socket(Sock, "SUBJECT:"++ Subject ++ "\r\n").

%% send email data
send_email_data(Sock, Email) when Email#email_config.text       =/= undefined;
    Email#email_config.html       =/= undefined;
    Email#email_config.attachment =/= undefined ->
    send_socket(Sock, "MIME-VERSION: 1.0\r\n"),
    send_socket(Sock, "CONTENT-TYPE: multipart/mixed; BOUNDARY=\"#BOUNDARY#\"\r\n"),
    send_socket(Sock, "\r\n"),
    case Email#email_config.text of
        undefined -> nothing_to_do;
        _         -> send_email_text("text/plain", Email#email_config.text, Sock)
    end,
    case Email#email_config.html of
        undefined -> nothing_to_do;
        _         -> send_email_text("text/html", Email#email_config.html, Sock)
    end,
    case Email#email_config.attachment of
        undefined -> nothing_to_do;
        _         -> send_email_attachment("application/msword", Email#email_config.attachment, Sock)
    end;
send_email_data(_Sock, _Email) ->
    ok.

end_email(Sock) ->
    send_socket(Sock, "\r\n.\r\n"),
    recv_socket(Sock),
    send_socket(Sock, "QUIT\r\n"),
    recv_socket(Sock).

%% send email text
send_email_text(Type, FilePath, Sock) ->
    send_socket(Sock, "--#BOUNDARY#\r\n"),
    send_socket(Sock, "CONTENT-TYPE: "),
    send_socket(Sock, Type),
    send_socket(Sock, "\r\n\r\n"),

    {ok, Fd} = file:open(FilePath, [binary, read]),
    send_file_to_email(Sock, Fd, -1),
    ok = file:close(Fd),
    send_socket(Sock, "\r\n\r\n").

%% send email other type
send_email_attachment(_Type, [], _Sock) ->
    nothing_to_return;
send_email_attachment(Type, [FilePath | Rest], Sock) ->
    send_socket(Sock, "--#BOUNDARY#\r\n"),
    send_socket(Sock, "CONTENT-TYPE: "),
    send_socket(Sock, Type),
    send_socket(Sock, "; NAME="),
    send_socket(Sock, basename(FilePath)),
    send_socket(Sock, "\r\n"),
    send_socket(Sock, "CONTENT-TRANSFER-ENCODING: base64\r\n"),
    send_socket(Sock, "\r\n"),

    case file:open(FilePath, [binary, read]) of
        {ok, Fd} ->
            lager:info("Client: Send ~p ~p to server....", [Fd, FilePath]),
            send_file_to_email(Sock, Fd, 0),
            ok = file:close(Fd),
            send_socket(Sock, "\r\n\r\n"),
            send_email_attachment(Type, Rest, Sock);
        {error, Reason} ->
            lager:error("send email open file failed [~p]", [Reason])
    end.


%% send file
send_file_to_email(Sock, Fd, Base64Flag) ->
    case file:read(Fd, ?MAX_SIZE) of
        {ok, Data} ->
            case Base64Flag of
                -1 -> ok = send(Sock, Data);
                0  -> ok = send(Sock, base64:encode(Data))
            end,
            send_file_to_email(Sock, Fd, Base64Flag);
        eof             -> eof;
        {error, Reason} -> lager:error("read failed: ~p~n", [Reason])
    end.

%% her email address
rcpt_to_emails(_Sock, []) ->
    ok;
rcpt_to_emails(Sock, [ToEmail | Rest]) ->
    send_socket(Sock, "RCPT TO <" ++ ToEmail ++ ">\r\n"),
    rcpt_to_emails(Sock, Rest).

%% send socket
send_socket(Sock, Data) when is_list(Data)->
    send_socket(Sock, unicode:characters_to_binary(Data));
send_socket(Sock, Data) when is_binary(Data)->
    lager:info("Client: ~p", [Data]),
    ok = send(Sock, Data).

%% recv socket
recv_socket(Sock) ->
    case recv(Sock, 0) of
        {ok   , Packet} -> lager:info("Server: ~p", [binary_to_list(Packet)]);
        {error, Reason} -> lager:error("Server: recv failed: ~p", [Reason])
    end.

%% send data to server via tcp or ssl
send(Sock, Data) when Sock#socket.type =:= tcp ->
    gen_tcp:send(Sock#socket.sock, Data);
send(Sock, Data) when Sock#socket.type =:= ssl ->
    ssl:send(Sock#socket.sock, Data).

%% recv data to server via tcp or ssl
recv(Sock, Opinion) when Sock#socket.type =:= tcp ->
    gen_tcp:recv(Sock#socket.sock, Opinion);
recv(Sock, Opinion) when Sock#socket.type =:= ssl ->
    ssl:recv(Sock#socket.sock, Opinion).

%% 取出二进制或列表里对应下标(从0开始)的值
index(Data, Index) when is_binary(Data) ->
    index(binary_to_list(Data), Index);
index(Data, Index)
    when is_list(Data), is_integer(Index),
    Index >= 0, Index < length(Data) ->
    index(Data, Index, 0).

index([D0 | _DRest], GivenIndex, CurrIndex)
    when GivenIndex =:= CurrIndex ->
    D0;
index([_D0 | DRest], GivenIndex, CurrIndex) ->
    index(DRest, GivenIndex, CurrIndex + 1).

%% 将二进制Bin按数字Digit分隔成左右两个二进制数
split_binary_by_digit(Bin, Digit)
    when is_binary(Bin),is_integer(Digit) ->
    split_binary_by_digit(Bin, Digit, <<>>).

split_binary_by_digit(<<>>, _Digit, _Left) ->
    {error, unmatched};
split_binary_by_digit(<<N, Rest/binary>>, Digit, Left) when N =/= Digit ->
    split_binary_by_digit(Rest, Digit, <<Left/binary, N>>);
split_binary_by_digit(<<_N, Rest/binary>>, _Digit, Left) ->
    {ok, Left, Rest}.

%% 取出文件路径中的文件名
basename(FilePath) when is_list(FilePath) ->
    case lists:member($/, FilePath) of
        false -> FilePath;
        true  -> basename(lists:reverse(FilePath), [])
    end.

basename([C | _Rest], BaseName) when C =:= $/ ->
    BaseName;
basename([C | Rest], BaseName) ->
    basename(Rest, [C | BaseName]).

get_local_timestr(Date, Time) ->
    {Y,M,D} = Date,
    {H,M1,S} = Time,
    lists:flatten(io_lib:format("~p_~p_~p_~p_~p_~p_",[Y,M,D, H, M1, S])).

get_local_timestamp() ->
    {T1, T2, _} = erlang:timestamp(),
    T1 * 1000000 + T2.

get_timestamp (DateTime) ->
    UtcTime = erlang:localtime_to_universaltime(DateTime),

    %% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {00, 00, 00}})
    calendar:datetime_to_gregorian_seconds(UtcTime) - 62167219200.

local_timestamp_to_datatime (LocalTimeStamp) ->
    %% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970, 01, 01}, {00, 00, 00}})

    erlang:universaltime_to_localtime(
        calendar:gregorian_seconds_to_datetime(LocalTimeStamp + 62167219200)
    ).

init_config(Config) ->
    {log_dir, LogDirList}      = lists:keyfind(log_dir,      1, Config),
    {send_ip, SendIP}          = lists:keyfind(send_ip,      1, Config),
    {send_account, SendCount}  = lists:keyfind(send_account, 1, Config),
    {password, Password}       = lists:keyfind(password,     1, Config),
    {to_emails, ToEmails}      = lists:keyfind(to_emails,    1, Config),
    {subject, Subject}         = lists:keyfind(subject,      1, Config),
    {send_time, SendTime}      = lists:keyfind(send_time,    1, Config),
    {send_flag, SendFlag}      = lists:keyfind(send_flag,    1, Config),
    {is_debug,DebugFlag}       = lists:keyfind(is_debug,     1, Config),

    {TimeInterval, SendTimeList} = SendTime,

    #email_config
    {
        server_ip    = SendIP,
        account      = SendCount,
        password     = Password,
        subject      = Subject,
        to_emails    = ToEmails,
        logdir       = LogDirList,
        timeinterval = TimeInterval,
        send_time    = SendTimeList,
        send_flag    = SendFlag,
        is_debug     = DebugFlag
    }.

move_log_file(FileName,TarDir) ->
    Command = "mv "++ FileName ++" "++TarDir,
    os:cmd(Command).

pack_log_files(LogDirList)->
    lists:foldl(fun({FileName, FileDir,TarDir}, T)->
        {Date,Time} = erlang:localtime(),
        DateStr  = get_local_timestr(Date, Time),
        Command = "tar -zcvf " ++DateStr++FileName ++ " " ++ FileDir,
        os:cmd(Command),
        LogTarName = DateStr ++ FileName,
        move_log_file(LogTarName,TarDir),
        LogTar = TarDir ++ LogTarName,
        T ++ [LogTar]
    end,
        [],
        LogDirList
    ).

start_to_send_email(Config) ->
    {Date, Time} = erlang:localtime(),
    NewConfig = Config #email_config{
        attachment = pack_log_files(Config #email_config.logdir),
        subject    = get_local_timestr(Date, Time) ++ Config #email_config.subject
    },
    lager:info("start to send log email [~p] to [~p]",[NewConfig #email_config.attachment, NewConfig #email_config.to_emails]),
    send(NewConfig).


#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang
%% The purpose of this script is to create the config file from the "server.conf" file


%% 配置文件名
-define(CONFIGFILENAME, ".conf").

%% 默认配置
-define(DEFAULTCONFIG, [
                            "## Cookie for distributed erlang\n",
                            "-setcookie knowledge",
                            "\n\n",

                            "## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive\n",
                            "## (Disabled by default..use with caution!)\n",
                            "-heart",
                            "\n\n",

                            "## Enable kernel poll and a few async threads\n",
                            "##+K true\n",
                            "##+A 5\n",

                            "-hidden",
                            "\n\n",

                            "## Increase number of concurrent ports/sockets\n",
                            "##-env ERL_MAX_PORTS 4096",
                            "\n\n",

                            "## Tweak GC to run more often\n",
                            "##-env ERL_FULLSWEEP_AFTER 10\n\n"
                            ]).

%% 获取配置列表
get_config(ConfigFlag) ->
     ConfigFileName = ConfigFlag ++ ?CONFIGFILENAME,
     ConfigList = case file:consult(ConfigFileName) of
            {ok, Config}    -> lists:flatten(Config);
            {error, Reason} ->
                io:format("read config file error ~p...~n", [Reason]),
                []
     end,

     ConfigList.

create_config_file(FilePath, Config) ->
        AddConfig = lists:foldl(
            fun({Key,Val}, T) ->
                T ++ Key ++ " " ++ Val ++ "\n\n"
            end,
            [],
            Config
        ),

        ConfigContents = ?DEFAULTCONFIG ++ AddConfig,

        case file:read_file(FilePath) of
                {ok, _ContentsBin} ->
                    io:format("has ~p, update the file successfully !!!~n", [FilePath]);
                _ ->
                    io:format("create a new ~p successfully !!! ~n", [FilePath])
        end,

        file:write_file(FilePath, ConfigContents),

        FilePath.


update_app_file(FilePath, Config) ->
    case file:consult(FilePath) of
        {ok, Src} ->
            io:format("update app file src:~p", [Src]),
            [{A,M,SrcContents}] = Src,
            OldEnv = lists:keyfind(env,1,SrcContents),
            if OldEnv =:= false
                ->
                    exit(get_env_failed);
                true ->
                    {env, OldConfig} = OldEnv,
                    NewEnvConfig = lists:foldl(fun({Key,Val},NewConfig)->
                            Result = case lists:keyfind(list_to_atom(Key), 1, NewConfig) of
                                false   -> NewConfig ++ [{list_to_atom(Key), Val}];
                                _OldVal -> N = lists:keydelete(list_to_atom(Key),1,NewConfig),
                                          N ++ [{list_to_atom(Key), Val}]
                            end,
                            Result
                        end,
                        OldConfig,
                        Config
                    ),
                    New = lists:keyreplace(env, 1, SrcContents,  {env,NewEnvConfig}),
                    NewSrc = {A,M,New},
                    {ok, S} = file:open(FilePath, write),
                    io:format(S, "%% app generated at ~p ~p~n", [date(), time()]),
                    io:format(S, "~p.~n", [NewSrc]),
                    file:close(S)
            end,
            FilePath;
        {error, Reason} ->
            exit(Reason)
    end.

do_config(ConfigFlag, Module)->
     ConfigList = get_config(ConfigFlag),
        Mod = list_to_atom(Module),
        List = lists:foldl(
                fun({ServerName,FilePath,Config}, T) when is_list(Config), Config =/= [] ->
                    if ServerName =:= Mod
                         ->
                           %% 特殊处理app文件
                           try update_app_file(FilePath, Config) of
                               SuccessFile -> T ++ [SuccessFile]
                           catch
                               exit:Reason -> io:format("create server [~p] app config file failed:~p ~n", [ServerName, Reason]), T
                           end;
                        true  ->
                           T
                    end
                end,
                [],
                ConfigList),
            if
                is_list(List) andalso List =/= [] ->
                    io:format("~p ~nsuccessfully!!! ~n",[List]);
                true ->
                    io:format("all failed !!! ~n")
            end.

do_config(ConfigFlag)->
     ConfigList = get_config(ConfigFlag),
        io:format("create config files .....~n"),
        List = lists:foldl(
            fun({ServerName,FilePath,Config}, T) when is_list(Config), Config =/= [] ->
                if ServerName =:= gate_app orelse ServerName =:= db_app orelse ServerName =:= game_app orelse ServerName =:= dgry_admin
                     ->
                       %% 特殊处理app文件
                       try update_app_file(FilePath, Config) of
                           SuccessFile -> T ++ [SuccessFile]
                       catch
                           exit:Reason -> io:format("create server [~p] app config file failed:~p ~n", [ServerName, Reason]), T
                       end;
                   ServerName =:= gate orelse ServerName =:= db orelse ServerName =:= game
                     ->
                       %%  处理vmgs文件
                       try create_config_file(FilePath, Config) of
                           SuccessFile -> T ++ [SuccessFile]
                       catch
                           exit:Reason -> io:format("create server [~p] vm.args config file failed:~p ~n", [ServerName, Reason]), T
                       end;
                    true ->
                       T
                end
            end,
            [],
            ConfigList),
        if
            is_list(List) andalso List =/= [] ->
                io:format("~p ~nsuccessfully!!! ~n",[List]);
            true ->
                io:format("all failed !!! ~n")
        end.

main([Args]) ->
    Split = string:tokens(Args,"-"),
    if
        length(Split) >= 2  ->
            [ConfigFlag,Module] = Split,
            do_config(ConfigFlag, Module);
        true ->
            [ConfigFlag] = Split,
            do_config(ConfigFlag)
    end.






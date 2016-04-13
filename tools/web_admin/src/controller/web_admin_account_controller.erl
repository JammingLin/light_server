-module(web_admin_account_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").
-define(PWD_SALT, "dgry,be,win").

logon('GET', []) ->
    GMServers = web_admin_base:get_gm_server_names(),
    ?REPLY_OK(#{gm_servers => GMServers});

logon('POST', []) ->
    lager:info("SessionID ----------- ~p", [SessionID]),
    ServerName = Req:post_param("server"),
    Account = Req:post_param("account"),
    Password = Req:post_param("password"),
    RememberMe = Req:post_param("rememberme"),
    ReturnUrl = Req:query_param("returnurl"),

    try
        {IPA, IPB, IPC, IPD} = Req:peer_ip(),
        lager:info("some one logon from ~p.~p.~p.~p", [IPA, IPB, IPC, IPD]),
        ok = handle_logon(ServerName, Account, Password, RememberMe),
        NewReturnUrl = if
                           Account =:= "Translator" ->
                               "/feedback/list?feedback_type=1&page_no=1&page_size=20";
                               true->
                                ReturnUrl
        end,
            lager:info("returnurl ~p",[NewReturnUrl]),
        case NewReturnUrl of
            "undefined" ->
                {redirect, "/"};
            _ ->
                {redirect, NewReturnUrl}
        end
    catch
        C:R ->
            GMServers = web_admin_base:get_gm_server_names(),
            lager:error("c:~p, r:~p", [C, R]),
            ?REPLY_ERROR({R, #{gm_servers => GMServers}})
    end.

logout('GET', []) ->
    boss_session:delete_session(SessionID),
    {redirect, "/"}.

info('GET', []) ->
    case boss_session:get_session_data(SessionID, "User") of
        undefined ->
            ?REPLY_JSON({error, 0, "unlogin"});
        User ->
            ?REPLY_JSON(User)
    end.

modify_password('GET', []) ->
    ?REPLY_OK("");

modify_password('POST', []) ->
    try
        OldPwd = Req:post_param("password"),
        NewPwd1 = Req:post_param("password_new"),
        NewPwd2 = Req:post_param("password_new2"),
        handle_modify_password(OldPwd, NewPwd1, NewPwd2),
        ?REPLY_OK(modify_success)
    catch
        throw:R ->
            ?REPLY_OK({error, 0, R})
    end.

handle_modify_password(OldPwd, NewPwd1, NewPwd2) ->
    UserInSession = boss_session:get_session_data(SessionID, "User"),
    User = boss_db:find(UserInSession:id()),
    check_new_pwd(NewPwd1, NewPwd2),
    check_old_pwd(User, OldPwd),
    NewPwd = md5_string:md5_hex(NewPwd1 ++ ?PWD_SALT),
    User1 = User:set(pwd, NewPwd),
    User1:save().

check_new_pwd(NewPwd1, NewPwd2) ->
    case NewPwd1 of
        NewPwd2 ->
            ok;
        _ ->
            throw({pwd1_pwd2})
    end.

check_old_pwd(User, OldPwdInput) ->
    OldPwd = md5_string:md5_hex(OldPwdInput ++ ?PWD_SALT),
    OP = binary_to_list(User:pwd()),
    case OldPwd of
        OP ->
            ok;
        _ ->
            lager:error("~p,              ~p", [OP, OldPwd]),
            throw({invalid_account_or_password})
    end.

role_info('GET', []) ->
    User = boss_session:get_session_data(SessionID, "User"),
    Roles = User:roles(),
    Modules = User:modules(),
    lager:debug("modules:~p", [Modules]),
    ?REPLY_OK(#{roles => Roles, modules => Modules}).

not_enough_role('GET', []) ->
    lager:info("**** ~p", [Req:header("Accept")]),
    case Req:header("X-Requested-With") of
        "XMLHttpRequest" ->
            ?REPLY_JSON({error, 500, not_enough_role});
        _ ->
            ?REPLY_OK(#{uri => Req:query_param("uri")})
    end.

handle_logon(ServerName, Account, Password, _RememberMe) ->
    PwdMd5 = md5_string:md5_hex(Password ++ ?PWD_SALT),
    case boss_db:find(admin_user, [{account, Account}, {pwd, PwdMd5}]) of
        [User] ->
            UserNoPwd = User:set(pwd, "******"),
            ServerData = web_admin_base:get_gm_server_data(ServerName),
            ensure_server_enable(ServerData),
            boss_session:set_session_data(SessionID, "ServerName", ServerName),
            boss_session:set_session_data(SessionID, "Server", ServerData),
            boss_session:set_session_data(SessionID, "User", UserNoPwd),
            boss_session:set_session_data(SessionID, "ModuleActions", User:module_actions()),

            boss_session:set_session_data(SessionID, "Role", 3),
            ok;
        db_connection_down ->
            throw({db_connection_down});
        _ ->
            throw({invalid_account_or_password})
    end.

ensure_server_enable(ServerData) ->
    Nodes = maps:values(ServerData),
    case [net_adm:ping(Node) || Node <- Nodes] of
        [pong] ->
            ok;
        Else ->
            lager:error("could_not_connect_server :~p   , ~p", [Nodes, Else]),
            throw({could_not_connect_server})
    end.
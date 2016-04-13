-module(web_admin_logview_controller, [Req, SessionID]).
-compile(export_all).

-include("../../include/web_admin.hrl").

before_(Action, _Method, _UrlTokens) ->
    web_admin_base:auth(?MODULE, Action, Req, SessionID).

index('GET', []) ->
    ?REPLY_OK([]).

log_files('GET', []) ->
    App = Req:query_param("app"),
    {Node, Suffix} = get_node_suffix(App),
    {ok, Path} = get_log_path(Node, Suffix),
    {ok, Files} = get_log_files(Node, Path),
    ?REPLY_JSON(#{files => [list_to_binary(F) || F <- Files]}).

read_log_file('GET', []) ->
    App = Req:query_param("app"),
    File = Req:query_param("file"),
    {Node, Suffix} = get_node_suffix(App),
    {ok, Path} = get_log_path(Node, Suffix),
    FileSafe = utils:safe_filename(File),
    {ok, TextBin} = read_log_file(Node, Path, FileSafe),
    ?REPLY_JSON(#{text => TextBin}).

get_node_suffix(App) ->
    case App of
        "db" ->
            {web_admin_base:db_node(SessionID), "../log/db"};
        "gw" ->
            {web_admin_base:gate_node(SessionID), "../log/gate"};
        "gs" ->
            {web_admin_base:game_node(SessionID), "../log/dgry"};
        "gt" ->
            {node(), ""}
    end.

get_log_path(Node, Suffix) ->
    {ok, LogPath} = rpc:call(Node, application, get_env, [lager, log_root]),
    {ok, Cwd} = rpc:call(Node, file, get_cwd, []),
    lager:info("node:~p, cwd:~p, path:~p, suffix:~p", [Node, Cwd, LogPath, Suffix]),
    Path = Cwd ++ "/" ++ LogPath ++ Suffix,
    {ok, Path}.

get_log_files(Node, Path) ->
    {ok, Files} = rpc:call(Node, file, list_dir, [Path]),
    lager:info("fiels:~p", [Files]),
    Files1 = lists:sort(Files),
    {ok, Files1}.

read_log_file(Node, Path, File) ->
    {ok, TextBin} = rpc:call(Node, file, read_file,[Path ++ "/" ++ File]),
    {ok, TextBin}.

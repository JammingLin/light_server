%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2015 下午3:32
%%%-------------------------------------------------------------------
-module(admin_user, [Id, Account, Pwd]).
-has({admin_user_roles, many}).
-compile(export_all).
-author("chendh").


roles() ->
    User = boss_db:find(id()),
    UserRoles = User:admin_user_roles(),
    [boss_db:find(UserRole:role_id()) || UserRole <- UserRoles].

modules() ->
    User = boss_db:find(id()),
    UserRoles = User:admin_user_roles(),
    RoleModuleSets = [boss_db:find(role_module, [{role_id, UserRole:role_id()}]) || UserRole <- UserRoles],
    RoleModules = lists:flatten(RoleModuleSets),
    [begin
         ModuleId = RoleModule:module_id(),
         boss_db:find(ModuleId)
     end || RoleModule <- RoleModules].

module_uris() ->
    Modules = modules(),
    [binary_to_list(M:uri()) || M <- Modules].

module_actions() ->
    Modules = modules(),
    [{binary_to_list(M:controller()), binary_to_list(M:action())} || M <- Modules].

is_super() ->
    User = boss_db:find(id()),
    User:account() == <<"admin">>.

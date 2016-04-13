%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2015 下午3:32
%%%-------------------------------------------------------------------
-module(admin_user_role, [Id, AdminUserId, RoleId]).
-belongs_to(admin_user).
-belongs_to(role).
-compile(export_all).
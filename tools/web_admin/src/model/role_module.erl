%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2015 下午3:32
%%%-------------------------------------------------------------------
-module(role_module, [Id, RoleId, ModuleId]).
-belongs_to(role).
-belongs_to(module).
-compile(export_all).
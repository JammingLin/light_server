%%%-------------------------------------------------------------------
%%% @author jie
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. May 2015 下午3:32
%%%-------------------------------------------------------------------
-module(role, [Id, Name, Desc]).
-has({admin_user_roles, many}).
-compile(export_all).
-author("chendh").
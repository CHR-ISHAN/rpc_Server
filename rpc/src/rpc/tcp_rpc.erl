%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2019 11:06
%%%-------------------------------------------------------------------
-module(tcp_rpc).
-author("dhcd").

%% API
-export([
  call/5,
  cast/5
]).

call(Name, Ip,Mod, Function, Args) ->
  rpc_server:call(Name, Ip, 8650, Mod, Function, Args).

cast(Name, Ip, Mod, Function, Args) ->
  rpc_server:cast(Name, Ip, 8650, Mod, Function, Args).




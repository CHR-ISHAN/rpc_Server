%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2019 15:28
%%%-------------------------------------------------------------------
-module(rpc_sup).
-author("dhcd").

-behaviour(supervisor).

%% API
-export([start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  lager:info("mod_rpc_sup: start_link"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  lager:info("rpc init"),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  AChild = {'rpc_server_sup', {'rpc_server_sup', start_link, []},
    Restart, Shutdown, supervisor, ['rpc_server_sup']},
  Rpc_Acceptor = {'rpc_accept', {'rpc_accept', start_link, []},
    Restart, Shutdown, Type, ['rpc_accept']},
  Rpc_Mod = {'rpc_mod_sup', {'rpc_mod_sup', start_link, []},
    Restart, Shutdown, supervisor, ['rpc_mod_sup']},

  {ok, {SupFlags, [AChild,Rpc_Mod,Rpc_Acceptor]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

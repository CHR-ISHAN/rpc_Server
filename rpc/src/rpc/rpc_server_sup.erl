%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jul 2019 16:18
%%%-------------------------------------------------------------------
-module(rpc_server_sup).
-author("dhcd").

-behaviour(supervisor).

%% API
-export([start_link/0,
  start_client/3]).

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
  lager:info("rpc_server_sup start"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'rpc_server', {'rpc_server', start_link, []},
    Restart, Shutdown, Type, ['rpc_server']},

  {ok, {SupFlags, [AChild]}}.

start_client(Name,Ip, Port) ->
  case supervisor:start_child(?MODULE, [Name,Ip, Port]) of
    {ok, Pid} ->
      %lager:info("mod_rpc_server_sup:rpc_server_sup Pid :~p ", [Pid]),
      {ok, Pid};
    {already_started, Pid} -> {ok, Pid};
    Err -> lager:info("mod_rpc_server_sup:rpc_server_sup Err : ~p, ?MOUDLE : ~p", [Err, ?MODULE]), Err
  end.
%%%===================================================================
%%% Internal functions
%%%===================================================================

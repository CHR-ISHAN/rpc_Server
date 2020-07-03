%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2019 16:20
%%%-------------------------------------------------------------------
-module(rpc_server).
-author("dhcd").

-behaviour(gen_server).

%% API
-export([start_link/3,
  call/6,
  cast/6,
  the_pid/3
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(SocketOpts, [binary, {packet, 4},
  {active, true}, {reuseaddr, true}, {nodelay, true},
  {delay_send, false}, {send_timeout, 5000}, {keepalive, true}]).

-record(state, {sock, name, ip, port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(Name, Ip, Port) ->
  case gen_server:start_link(?MODULE, [Name, Ip, Port], []) of
    {ok, Pid} ->
      lager:info("mod_rpc_server:rpc_server Pid : ~p Ip : ~p", [Pid, Ip]),
      {ok, Pid};
    Err ->
      lager:info("mod_rpc_server:Error: ~p,Name: ~p,Port: ~p", [Err, Port]),
      Err
  end.

call(Name, Ip, Port, Mod, Function, Args) ->
  case the_pid(Name, Ip, Port) of
    Pid when is_pid(Pid) ->
      lager:info("mod_rpc_server:The new new Pid : ~p ", [Pid]),
      gen_server:call(Pid, {call, Name, Mod, Function, Args});
    _ ->
      {error}
  end.

cast(Name, Ip, Port, Mod, Function, Args) ->
  case the_pid(Name, Ip, Port) of
    Pid when is_pid(Pid) ->
      lager:info("mod_rpc_server:The Pid is :~p", [Pid]),
      gen_server:cast(Pid, {cast, Mod, Function, Args});
    _ ->
      {error}
  end.
the_pid(Name, Ip, Port) ->
  Gproc = gproc:where({n, l, Name}),
  case Gproc of
    undefined ->
      Other = rpc_server_sup:start_client(Name, Ip, Port),
      case Other of
        {ok, Pid} -> lager:info("mod_rpc_server:the Pid  is is : ~p", [Pid]), Pid;
        _ -> lager:info("mod_rpc_server:The_Pid Other is ~p", [Other]), undefined
      end;
    Pid -> Pid
  end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Name, Ip, Port]) ->
  case gen_tcp:connect(Ip, Port, ?SocketOpts) of
    {ok, Sock} -> gproc:reg({n, l, Name}), {ok, #state{sock = Sock, ip = Ip, port = Port}};
    Err -> {stop, Err}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({call, _Name, _Mod, _Function, _Args} = Req, From, State) ->
  case gen_tcp:send(State#state.sock, term_to_binary({Req, From})) of
    ok -> {noreply, State};
    Err ->
      lager:info("mod_rpc_server:send: ~p,error: ~p", [Req, Err]),
      {stop, normal, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({cast, _Mod, _Function, _Args} = Req, State) ->
  gen_tcp:send(State#state.sock, term_to_binary({Req, undefined})),
  ok;
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, _Socket, Data}, State) ->
  case binary_to_term(Data) of
    {reply, Reply, Caller} ->
      lager:info("Reply:~p,Caller :~p", [Reply, Caller]),
      gen_server:reply(Caller, Reply);
    _ -> ok
  end,
  {noreply, State};
handle_info({tcp_close, _Socket}, State) ->
  lager:info("mod_rpc_server:tcp close: ~p", [State]),
  {stop, noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2014 18:14
%%%-------------------------------------------------------------------
-module(ph_socks5_server).
-author("anna").

-behaviour(gen_server).

%% API
-export([
  start_link/4,
  stop/1,
  send_to_client/2,
  close_connection/1
]).

%% gen_server callbacks
%% init/1 callback does not exports because of using gen_server:enter_loop/4
-export([
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% init function for proc_lib:start_link/3
-export([pl_init/4]).

-define(SERVER, ?MODULE).

-include("socks5.hrl").

-record(state, {ref, client_sock, transport, status, auth_method, dest_sock, udp_sock, idle_time, users_table}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, ClientSock, Transport, Opts) ->
  {ok, Pid} = proc_lib:start_link(?MODULE, pl_init, [Ref, ClientSock, Transport, Opts]),
  {ok, {ClientHost, _ClientPort}} = inet:peername(ClientSock),
  lager:info("Client ~s connected to socks5 server", [ipv4_to_string(ClientHost)]),
  {ok, Pid}.

pl_init(Ref, ClientSock, Transport, _Opts = [UsersTable, IdleTime]) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = Transport:setopts(ClientSock, [{active, once}]),
  %% Triggers self() process to be the ClientSocket's owner
  ok = ranch:accept_ack(Ref),
  gen_server:enter_loop(?MODULE, [], #state{ref = Ref, client_sock = ClientSock, transport = Transport,
    status = auth_method, auth_method = ?NO_AUTH, idle_time = IdleTime, users_table = UsersTable}, IdleTime).

%%--------------------------------------------------------------------
%% Stops process
stop(Pid) ->
  gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% Sends data to client
send_to_client(Pid, Data) ->
  gen_server:cast(Pid, {send_to_client, Data}).

%%--------------------------------------------------------------------
%% Closes connection with client
close_connection(Pid) ->
  gen_server:cast(Pid, close_connection).
%%--------------------------------------------------------------------
parse_request(Pid, RawReq) ->
  gen_server:cast(Pid, {parse_request, RawReq}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% No meaningful handle_call callbacks
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------

handle_cast({send_to_client, Data}, State = #state{client_sock = ClientSock, transport = Transport,
    idle_time = IdleTime}) ->
  ok = Transport:send(ClientSock, Data),
  {noreply, State, IdleTime};

handle_cast({parse_request, <<?SOCKS_VER, _NumMeth, Methods/binary>>}, State = #state{status = auth_method,
    idle_time = IdleTime}) ->
  {method, AuthMethod, Status} = choose_auth_method(binary_to_list(Methods)),
  send_to_client(self(), <<?SOCKS_VER, AuthMethod>>),
  {noreply, State#state{status = Status, auth_method = AuthMethod}, IdleTime};

handle_cast({parse_request, <<?SOCKS_VER, Cmd, ?RSV, Atyp, Address/binary>>},
    State = #state{client_sock = ClientSock, idle_time = IdleTime, status = Status}) ->
  {ok, {ClientHost, _ClientPort}} = inet:peername(ClientSock),
  ClientIpStr = ipv4_to_string(ClientHost),
  Result = parse_addr(Atyp, Address),

  case eval_request(Cmd, Result, Status) of
    {connect, {ok, DestSock}} ->
      ok = inet:setopts(DestSock, [{active, once}]),
      {ok, {DestHost, DestPort}} = inet:peername(DestSock),
      DestHostBin = list_to_binary(tuple_to_list(DestHost)),
      lager:info("Client ~s connected to host ~s on port ~w", [ClientIpStr, ipv4_to_string(DestHost), DestPort]),
      send_to_client(self(), <<?SOCKS_VER, ?SUCCESS, ?RSV, ?IPV4, DestHostBin:4/binary, DestPort:16/integer>>),
      {noreply, State#state{status = proxy_from_client, dest_sock = DestSock}, IdleTime};

    {bind, {ok, BindSock}} ->
      {ok, {BindHost, BindPort}} = inet:sockname(BindSock),
      BindHostBin = list_to_binary(tuple_to_list(BindHost)),
      lager:info("Client ~s binded socket on host ~s to port ~w", [ClientIpStr, ipv4_to_string(BindHost), BindPort]),
      send_to_client(self(), <<?SOCKS_VER, ?SUCCESS, ?RSV, ?IPV4, BindHostBin:4/binary, BindPort:16>>),
      {ok, _Pid} = ph_bind_server:start(self(), BindSock),
      {noreply, State, IdleTime};

    {_Any, {error, Reason}} ->
      {error, Rep} = errno_to_rep(Reason),
      lager:warning("Error ~w occured, closing connection to client ~s", [Rep, ClientIpStr]),
      send_to_client(self(), <<?SOCKS_VER, Rep, ?RSV, Atyp, Address/binary>>),
      close_connection(self()),
      {stop, normal, State}
  end;

handle_cast({parse_request, <<?UNAME_VER, ULen, UName:ULen/binary, PLen, Passwd:PLen/binary>>},
    State = #state{status = uname_auth, idle_time = IdleTime, users_table = UsersTable}) ->
  Res = ets:lookup(UsersTable, binary_to_list(UName)),
  case check_passwd(Res, binary_to_list(Passwd)) of
    ok ->
      send_to_client(self(), <<?UNAME_VER, ?SUCCESS>>),
      {noreply, State#state{status = socks_req}, IdleTime};
    error ->
      send_to_client(self(), <<?UNAME_VER, ?FAILURE>>),
      close_connection(self()),
      {stop, normal, State}
  end;

handle_cast({parse_request, Data}, State = #state{status = proxy_from_client, dest_sock = DestSock,
    idle_time = IdleTime}) ->
  ok = gen_tcp:send(DestSock, Data),
  {noreply, State, IdleTime};

handle_cast(close_connection, State = #state{client_sock = ClientSock, transport = Transport}) ->
  Transport:close(ClientSock),
  {stop, normal, State};

handle_cast(stop, State) ->
  {stop, normal, State}.

%%--------------------------------------------------------------------

handle_info({tcp, ClientSock, RawReq}, State = #state{client_sock = ClientSock, transport = Transport,
    idle_time = IdleTime}) ->
  ok = parse_request(self(), RawReq),
  ok = Transport:setopts(ClientSock, [{active, once}]),
  {noreply, State, IdleTime};

handle_info({tcp, DestSock, Data}, State = #state{dest_sock = DestSock, idle_time = IdleTime}) ->
  ok = send_to_client(self(), Data),
  ok = inet:setopts(DestSock, [{active, once}]),
  {noreply, State, IdleTime};

handle_info({bind_accepted, Host, Port}, State = #state{client_sock = ClientSock, idle_time = IdleTime}) ->
  {ok, {ClientHost, _ClientPort}} = inet:peername(ClientSock),
  lager:info("Client ~s got server-to-client connection with host ~s on port ~w", [ipv4_to_string(ClientHost),
    ipv4_to_string(Host), Port]),
  HostBin = list_to_binary(tuple_to_list(Host)),
  ok = send_to_client(self(), <<?SOCKS_VER, ?SUCCESS, ?RSV, ?IPV4, HostBin:4/binary, Port:16>>),
  {noreply, State, IdleTime};

handle_info({proxy_to_client, Data}, State = #state{idle_time = IdleTime}) ->
  ok = send_to_client(self(), Data),
  {noreply, State, IdleTime};

%% Stops the process because of closing TCP connection by client (or destination host).
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info(timeout, State = #state{client_sock = ClientSock, dest_sock = DestSock}) ->
  {ok, {ClientHost, _ClientPort}} = inet:sockname(ClientSock),
  {ok, {DestHost, DestPort}} = inet:sockname(DestSock),
  DestHostBin = list_to_binary(tuple_to_list(DestHost)),
  lager:warning("Error ~w occured, closing connection to client ~s", [?TTL_EXPIRED, ipv4_to_string(ClientHost)]),
  send_to_client(self(), <<?SOCKS_VER, ?TTL_EXPIRED, ?RSV, ?IPV4, DestHostBin:4/binary, DestPort:16>>),
  close_connection(self()),
  {stop, normal, State}.
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
choose_auth_method(Methods) ->
  Result = lists:member(?UNAME_PASSWD, Methods),
  if
    Result -> {method, ?UNAME_PASSWD, uname_auth};
    true -> choose_auth_method1(Methods)
  end.

choose_auth_method1([]) ->
  {method, ?NO_ACCEPTABLE_METH};
choose_auth_method1([Method|Tail]) ->
  choose_auth_method1(Method, Tail).

choose_auth_method1(?NO_AUTH, _Methods) ->
  {method, ?NO_AUTH, socks_req};
choose_auth_method1(?UNAME_PASSWD, _Methods) ->
  {method, ?UNAME_PASSWD, uname_auth};
choose_auth_method1(_Method, Methods) ->
  choose_auth_method1(Methods).

parse_addr(1, <<HostBin:4/binary, PortBin:2/binary>>) -> % IPv4
  Host = list_to_tuple(binary_to_list(HostBin)),
  <<Port:16>> = PortBin,
  {ok, Host, Port};
parse_addr(3, <<Octets, DomainPort/binary>>) -> % domain name
  <<Domain:Octets/binary, PortBin:2/binary>> = DomainPort,
  <<Port:16>> = PortBin,
  case inet_res:gethostbyname(binary_to_list(Domain)) of
    {ok, {hostent, _, _, _, _, [Host|_Tail]}} -> {ok, Host, Port};
    {error, Reason} -> {error, Reason}
  end;
parse_addr(_Atyp, _Addr) ->
  {error, eatypnotsupp}.

ipv4_to_string(Address) ->
  io_lib:format("~w.~w.~w.~w", tuple_to_list(Address)).

eval_request(?CONNECT, {ok, Host, Port}, socks_req) ->
  Res = gen_tcp:connect(Host, Port, []),
  {connect, Res};
eval_request(?BIND, {ok, Host, Port}, proxy_from_client) ->
  Res = gen_tcp:listen(Port, [{ip, Host}]),
  {bind, Res};
eval_request(?BIND, {ok, _Host, _Port}, _Status) ->
  {any, {error, egenfailure}};
eval_request(_Cmd, {ok, _Host, _Port}, _Status) ->
  {any, {error, ecmdnotsupp}};
eval_request(_Cmd, {error, Reason}, _Status) ->
  {any, {error, Reason}}.

errno_to_rep(enetunreach) ->
  {error, ?NET_UNREACH}; % network unreachable
errno_to_rep(ehostunreach) ->
  {error, ?HOST_UNREACH}; % host unreachable
errno_to_rep(econnrefused) ->
  {error, ?CONN_REFUSED}; % connection refused
errno_to_rep(ecmdnotsupp) ->
  {error, ?CMD_NOT_SUPPORT}; % command not supported
errno_to_rep(eatypnotsupp) ->
  {error, ?ATYP_NOT_SUPPORT}; % address type not supported
errno_to_rep(_Reason) ->
  {error, ?GEN_FAILURE}. % general SOCKS server failure


check_passwd([{_Uname, Salt, Hash}], Passwd) ->
  Hash1_ = crypto:hash(md5, Salt ++ Passwd),
  Hash1 = lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X:8>> <= Hash1_ ]),
  check_passwd(Hash, Hash1);
check_passwd(Hash, Hash) ->
  ok;
check_passwd([_Passwd1], _Passwd2) ->
  error;
check_passwd([], _Passwd) ->
  error;
check_passwd(_Hash1, _Hash2) ->
  error.


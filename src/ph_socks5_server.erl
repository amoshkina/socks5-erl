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
  reply/2,
  close_connection/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% init function for proc_lib:start_link/3
-export([pl_init/4]).

-define(SERVER, ?MODULE).

-include("socks5.hrl").

-record(state, {ref, client_sock, transport, status, auth_method, dest_sock}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, ClientSock, Transport, Opts) ->
  {ok, Pid} = proc_lib:start_link(?MODULE, pl_init, [Ref, ClientSock, Transport, Opts]),
  lager:info("Client ~s connected to socks5 server", [get_address(socket, ClientSock)]),
  {ok, Pid}.

pl_init(Ref, ClientSock, Transport, _Opts = []) ->
  ok = proc_lib:init_ack({ok, self()}),
  %% Sets Socket in active once mode to receive data from socket asynchronously only once (which not allows to flood
  %% the process' mailbox)
  ok = Transport:setopts(ClientSock, [{active, once}]),
  %% Triggers self() process to be the Socket's owner
  ok = ranch:accept_ack(Ref),
  %% TODO: maybe start some gen_fsm here
  gen_server:enter_loop(?MODULE, [], #state{ref = Ref, client_sock = ClientSock, transport = Transport,
    status = auth_method, auth_method = 0}).

%%--------------------------------------------------------------------
%% Stops process
stop(Pid) ->
  gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% Sends a reply to device
reply(Pid, Data) ->
  gen_server:cast(Pid, {reply, Data}).

%%--------------------------------------------------------------------
%% Closes connection with device
close_connection(Pid) ->
  gen_server:cast(Pid, close_connection),
  stop(Pid).
%%--------------------------------------------------------------------
parse_request(Pid, RawReq) ->
  gen_server:cast(Pid, {parse_request, RawReq}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, ClientSock, Transport, _Opts = []]) ->
  {ok, #state{ref = Ref, client_sock = ClientSock, transport = Transport}}.
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------

handle_cast({reply, Data}, State = #state{client_sock = ClientSock, transport = Transport}) ->
  Transport:send(ClientSock, Data),
  {noreply, State};

handle_cast(close_connection, State = #state{client_sock = ClientSock, transport = Transport}) ->
  Transport:close(ClientSock),
%%   lager:warning("Close connection"),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast({parse_request, <<?SOCKS_VER, _NumMeth, Methods/binary>>}, State = #state{status = auth_method}) ->
  {method, AuthMethod} = choose_auth_method(binary_to_list(Methods)),
  reply(self(), <<?SOCKS_VER, AuthMethod>>),
  {noreply, State#state{status = socks_req, auth_method = AuthMethod}};
handle_cast({parse_request, <<?SOCKS_VER, Cmd, 0, Atyp, Address/binary>>},
    State = #state{client_sock = ClientSock, status = socks_req}) ->
  Result = parse_addr(Atyp, Address),
  case eval_request(Cmd, Result) of
    {ok, DestSock} ->
      {ok, Host, Port} = Result,
      lager:info("Client ~s connected to host ~s on port ~w", [get_address(socket, ClientSock),
        get_address(ipv4, Host), Port]),
      reply(self(), <<?SOCKS_VER, ?SUCCESS, 0, Atyp, Address/binary>>),
      {noreply, State#state{status = proxy, dest_sock = DestSock}};

    {error, Rep} ->
      reply(self, <<?SOCKS_VER, Rep, 0, Atyp, Address/binary>>),
      close_connection(self()),
      {noreply, State}
  end;
handle_cast({parse_request, Data}, State = #state{status = proxy, dest_sock = DestSock}) ->
  ok = gen_tcp:send(DestSock, Data),
  {noreply, State}.

%%===================================================================================
%% TODO: move this block to internal functions

eval_request(1, {ok, Host, Port}) -> % TODO 2, 3
  gen_tcp:connect(Host,Port, []);
eval_request(_Cmd, {error, Reason}) ->
  errno_to_rep(Reason);

eval_request(_Cmd, {ok, _Host, _Port}) ->
  {error, 7}. % command not supported


% TODO: 02 connection not allowed by ruleset
% TODO: 06 TTL expired
errno_to_rep(enetunreach) ->
  {error, 3}; % network unreachable
errno_to_rep(ehostunreach) ->
  {error, 4}; % host unreachable
errno_to_rep(econnrefused) ->
  {error, 5}; % connection refused
errno_to_rep(eatypnotsupp) ->
  {error, 8}; % address type not supported
errno_to_rep(_Reason) ->
  {error, 1}. % general SOCKS server failure

%%===================================================================================



%%--------------------------------------------------------------------

handle_info({tcp, ClientSock, RawReq}, State = #state{client_sock = ClientSock, transport = Transport}) ->
%%   lager:info("RawReq = ~w", [RawReq]),
  parse_request(self(), RawReq),
%%   lager:info("Reply generated: ", [Reply]),
%%   reply(self(), Reply),
  ok = Transport:setopts(ClientSock, [{active, once}]),
  {noreply, State};

handle_info({tcp, DestSock, Data}, State = #state{dest_sock = DestSock, client_sock = ClientSock}) ->
  ok = gen_tcp:send(ClientSock, Data),
  {noreply, State};

%% Stops the process because of closing TCP connection by client (or destination host???). TODO ???
handle_info({tcp_closed, _Socket}, State) ->
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

choose_auth_method([]) ->
  {method, 16#FF};
choose_auth_method([Method|Tail]) ->
  Result = lists:member(Method, ?AUTH_METHODS),
  if
    Result -> {method, Method};
    true -> choose_auth_method(Tail)
  end.

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
%% parse_addr(4, _Ad) -> % IPv6 % TODO
%%   ok.
parse_addr(_Atyp, _Addr) ->
  {error, eatypnotsupp}.


get_address(socket, Socket) ->
  {ok, {Address, _Port}} = inet:peername(Socket),
  get_address(ipv4, Address);
get_address(ipv4, Address) ->
  io_lib:format("~w.~w.~w.~w", tuple_to_list(Address)).


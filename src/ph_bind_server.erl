%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2014 21:25
%%%-------------------------------------------------------------------
-module(ph_bind_server).
-author("anna").

-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ref, c_sock, monitor_ref}).

%%%===================================================================
%%% API
%%%===================================================================

start(Ref, BindSock) ->
  gen_server:start({local, ?SERVER}, ?MODULE, [Ref, BindSock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, BindSock]) ->
  MonitorRef = erlang:monitor(process, Ref),
  {ok, CSock} = gen_tcp:accept(BindSock),
  ok = inet:setopts(CSock, [{active, once}]),
  {ok, {PeerHost, PeerPort}} = inet:peername(CSock),
  Ref ! {bind_accepted, PeerHost, PeerPort},
  ok = gen_tcp:close(BindSock),
  {ok, #state{ref = Ref, c_sock = CSock, monitor_ref = MonitorRef}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, CSock, Data}, State = #state{ref = Ref, c_sock = CSock}) ->
  lager:warning("tcp received, ~w", [Data]),
  Ref ! {proxy_to_client, Data},
  {noreply, State};

handle_info({'DOWN', MonitorRef, process, _Object, _Info}, State = #state{c_sock = CSock, monitor_ref = MonitorRef}) ->
  lager:warning("Child listen stops"),
  ok = gen_tcp:close(CSock),
  {stop, normal, State};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

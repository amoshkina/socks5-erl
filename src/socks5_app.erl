%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2014 13:12
%%%-------------------------------------------------------------------
-module(socks5_app).
-author("anna").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  % getting parameters from config
  {ok, Socks5Port} = application:get_env(socks5_port),

  {ok, _} = ranch:start_listener(socks5_tcp, 1,
    ranch_tcp, [{port, Socks5Port}], ph_socks5_server, []),
  socks5_sup:start_link().

%%--------------------------------------------------------------------

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

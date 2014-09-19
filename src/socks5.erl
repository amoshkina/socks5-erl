%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2014 12:57
%%%-------------------------------------------------------------------
-module(socks5).
-author("anna").

%% API
-export([start/0]).

start() ->
  lager:start(),
  [application:start(App) || App <- [ranch, socks5]].


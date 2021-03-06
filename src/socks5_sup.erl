%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2014 13:20
%%%-------------------------------------------------------------------
-module(socks5_sup).
-author("anna").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

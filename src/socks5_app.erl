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
  {ok, IdleTime} = application:get_env(idle_time),
  {ok, UsersInfoFile} = application:get_env(users_info_file),
  UsersTable = users_table,
%%   UsersTable = dict:store("anonymous", "anonymous", dict:new()),
  {ok, Items} = file:consult(UsersInfoFile),
  ets:new(UsersTable, [set, named_table]),
  ok = fill_table(UsersTable, Items),

  {ok, _} = ranch:start_listener(socks5_tcp, 1,
    ranch_tcp, [{port, Socks5Port}], ph_socks5_server, [UsersTable, IdleTime]),
  socks5_sup:start_link().

%%--------------------------------------------------------------------

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fill_table(_Table, []) -> ok;
fill_table(Table, [Item|Tail]) ->
  ets:insert(Table, Item),
  fill_table(Table, Tail).

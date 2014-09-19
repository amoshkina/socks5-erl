%%%-------------------------------------------------------------------
%%% @author anna
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2014 13:07
%%%-------------------------------------------------------------------
-author("anna").

-define(SOCKS_VER, 5).
-define(AUTH_METHODS, [0, 1, 2]).

%% Server's replies after evaluating request
-define(SUCCESS, 0).
-define(GEN_FAILURE, 1).
-define(NOT_ALLOWED, 2).
-define(NET_UNREACH, 3).
-define(HOST_UNREACH, 4).
-define(CONN_REFUSED, 5).
-define(TTL_EXPIRED, 6).
-define(CMD_NOT_SUPPORT, 7).
-define(ATYP_NOT_SUPPORT, 8).

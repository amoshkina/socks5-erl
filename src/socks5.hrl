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
-define(AUTH_METHODS, [?NO_AUTH, ?UNAME_PASSWD]).

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

%% Commands
-define(CONNECT, 1).
-define(BIND, 2).
-define(UDP_ASSOCIATE, 3).

%% Address types
-define(IPV4, 1).
-define(DOMAINNAME, 3).
-define(IPV6, 4).

%% Reserved byte
-define(RSV, 0).

%% Authentification methods
-define(NO_AUTH, 0).
-define(GSSAPI, 1).
-define(UNAME_PASSWD, 2).
-define(NO_ACCEPTABLE_METH, 16#FF).

%% Username/Password
-define(UNAME_VER, 1).
-define(FAILURE, 16#FF).
%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements the utilities for mysqlmon.
%%%
%%% Utilities include
%%%     Communication Utils with the routing server
%%%
%%%
%%% @end
%%% Created : 13. Nov 2017 9:47 AM
%%%-------------------------------------------------------------------
-module(mysqlmon_util).
-author("bhanuka").

-include("mysqlmon.hrl").

%% API
-export([send_router/2, subscribe_service/2]).

send_router(ServiceName, Data) ->
	gen_server:call(mysqlmon_router_server, {ServiceName, Data}).

subscribe_service(Service, Pid) ->
	gen_server:call(mysqlmon_router_server, {subscribe, Service, Pid}).



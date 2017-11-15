%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements the utilities for mysqlmon.
%%%
%%% Utilities include
%%%     Communication Utils with the routing server
%%%     Data conversion Utilities
%%%
%%% @end
%%% Created : 13. Nov 2017 9:47 AM
%%%-------------------------------------------------------------------
-module(mysqlmon_util).
-author("bhanuka").

-include("mysqlmon.hrl").

%% API
-export([send_router/2, subscribe_service/2]).
-export([custom_date/1, custom_date/0]).

%%% Communication Utils
send_router(ServiceName, [Data]) ->
	send_router(ServiceName, Data);

send_router(ServiceName, [Data| Rest]) ->
	case send_router(ServiceName, Data) of
		ok ->
			send_router(ServiceName, Rest);
		_ ->
			send_router(ServiceName, lists:concat([[Data], Rest]))
	end;

send_router(ServiceName, Data) ->
	gen_server:call(mysqlmon_router_server, {ServiceName, Data}).

subscribe_service(Service, Pid) ->
	gen_server:call(mysqlmon_router_server, {subscribe, Service, Pid}).

%%% Data Conversion Utils
custom_date() ->
	custom_date(erlang:localtime()).

custom_date({{YYYY,MM,DD},{Hour, Min, Sec}}) ->
	lists:flatten(io_lib:format("~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[YYYY, MM, DD, Hour, Min, Sec])).


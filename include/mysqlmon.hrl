%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2017 11:53 AM
%%%-------------------------------------------------------------------
-author("bhanuka").

-define(APP_NAME, mysqlmon).

-define(INFO, 15).

-define(WARNING, 10).

-define(ERROR, 5).

-define(LOGMSG(_AppName, _MsgClass, Format, Params),
	io:fwrite(Format, Params)).

-define(MYSQL_PIDFILE_SERVICE_NAME, mysqlmon_pidfile).

-define(MYSQLMON_ROUTINGPG, mysqlmon_routing_pg).

-record(mysqlmon_event, {
	service = undefined,
	type    = undefined,
	reason  = undefined,
	description = "",
	data    = [],
	timestamp = erlang:localtime()
}).
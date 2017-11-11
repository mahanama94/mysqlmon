%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2017 11:53 AM
%%%-------------------------------------------------------------------
-author("bhanuka").

-define(APP_NAME, "mysqlmon").

-define(INFO, 15).

-define(WARNING, 10).

-define(ERROR, 5).

-define(LOGMSG(_AppName, _MsgClass, Format, Params),
	io:fwrite(Format, Params)).

-define(MYSQL_TXCOUNT, [
	{warn_threshold, 100},
	{crit_threshold, 1000},
	{check_interval, 100000}]).

-define(MYSQL_CONCOUNT, [
    {warn_threshold, 100},
    {crit_threshold, 1000},
    {check_interval, 100000},
	{dsn, ""}]).

-define(MYSQL_PIDFILE_SERVICE_NAME, mysqlmon_pidfile).

-define(MYSQL_PIDFILE, [
	{check_interval, 3000},
	{pidfile_path, "/var/run/mysqld/"},
	{pidfile_name, "mysqld.pid"}]).

-define(MYSQLMON_ROUTINGPG, mysqlmon_routing_pg).

-define(MYSQL_NDB_PIDFILE, [
	{check_interval, 3000},
	{pidfile_path, "/var/run/mysqld/"},
	{node_id, 1},
	{node_type, ndb} %% or cm
]).

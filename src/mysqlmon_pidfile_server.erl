%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module Implements periodic check for the mysqld pid file.
%%%
%%% File path can be specified in app configuration (pidfile_path) or the default path "/var/run/mysqld/"
%%% will be evaluated.
%%%
%%% Module checks the existence of mysqld.pid or pidfile_name specified in app env file
%%% in the specified path in specified intervals
%%%
%%% SPECIAL NOTES
%%% Run mode of the application requires the permission to view the file.
%%%
%%% @end
%%% Created : 09. Nov 2017 1:15 PM
%%%-------------------------------------------------------------------
-module(mysqlmon_pidfile_server).
-author("bhanuka").

-include("mysqlmon.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(SERVICE, ?MODULE).

-record(state, {service_name, pidfile_path, check_interval, pidfile_name}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Args) ->
	PidFilePath = proplists:get_value(pidfile_path, Args, "/var/run/mysqld/"),
	CheckInterval = proplists:get_value(check_interval, Args, 10000),
	PidFileName = proplists:get_value(pidfile_name, Args, "mysqld.pid"),
	ServiceName = proplists:get_value(service_name, Args, ?MYSQL_PIDFILE_SERVICE_NAME),
	{ok, #state{service_name = ServiceName, pidfile_path = PidFilePath, check_interval = CheckInterval, pidfile_name = PidFileName}, CheckInterval}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State) ->
  ?LOGMSG(?APP_NAME, ?WARNING, "~p | ~p usupported handle call Request : ~p From : ~p ~n", [?MODULE, ?LINE, Request, From]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
  ?LOGMSG(?APP_NAME, ?WARNING, "~p | ~p unsupported handle cast Request : ~p ~n", [?MODULE, ?LINE, Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(timeout, State) ->
	CheckInterval = State#state.check_interval,
	PidFileName = State#state.pidfile_name,
	FilePath = lists:concat([State#state.pidfile_path, PidFileName]),
	case file:open(FilePath, [read]) of
	{ok, IoDevice} ->
	    ?LOGMSG(?APP_NAME, ?INFO, "~p | ~p file exists IoDevice : ~p  ~n", [?MODULE, ?LINE, IoDevice]),
		case file:read_line(IoDevice) of
			{ok, LineData} ->
				[MysqlPid] = string:tokens(LineData, "\n"),
				CmdData  = string:tokens(os:cmd("ps -p "++ MysqlPid), "\n"),
				case length(CmdData) of
					1 ->
						mysqlmon_util:send_router(?SERVICE, process_error(MysqlPid, State)),
						?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql process doesn't exists Pid : ~p ~n", [?MODULE, ?LINE, MysqlPid]);
					_ ->
						?LOGMSG(?APP_NAME,?INFO, "~p | ~p mysql process exists Pid : ~p ~n", [?MODULE, ?LINE, MysqlPid])
				end ;
			{error, Reason} ->
				mysqlmon_util:send_router(?SERVICE, pidfile_error(Reason, State)),
				?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p Error Reading file Reason : ~p ~n", [?MODULE, ?LINE, Reason]);
			eof ->
				mysqlmon_util:send_router(?SERVICE, pidfile_error(eof, State)),
				?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p File is empty ~n", [?MODULE, ?LINE])
		end,
		file:close(IoDevice);
	{error, enoent} ->
		mysqlmon_util:send_router(?SERVICE, pidfile_error(enoent, State)),
		?LOGMSG(?APP_NAME, ?INFO, "~p | ~p file not found Reason : ~p ~n", [?MODULE, ?LINE, enoent]);
	{error, eacces} ->
		mysqlmon_util:send_router(?SERVICE, pidfile_error(eaccess, State)),
		?LOGMSG(?APP_NAME, ?INFO, "~p | ~p file access failed Reason : ~p ~n", [?MODULE, ?LINE, eaccess]);
	{error, Reason} ->
		mysqlmon_util:send_router(?SERVICE, pidfile_error(Reason, State)),
		?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql pidfile missing Reason : ~p Path ~p ~n", [?MODULE, ?LINE, Reason, FilePath])
	end,
	{noreply, State, CheckInterval};

handle_info(Info, State) ->
	?LOGMSG(?APP_NAME, ?WARNING, "~p | ~p unsupported handle_info Info : ~p ~n", [?MODULE, ?LINE,Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, _State) ->
  ?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p terminating mysqlmon_pidfile_server Reason : ~p ~n", [?MODULE, ?LINE, Reason]),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pidfile_error(Reason, State) ->
	Description =
		case Reason of
			eof ->
				"empty pid file";
			enoent ->
				"empty pid file";
			eaccess ->
				"error accessing";
			Other ->
				Other
		end,
	#mysqlmon_event{
		service = ?SERVICE,
		type = pidfile_error,
		description = Description,
		data = [
			{pidfile_path, State#state.pidfile_path}
		]
	}.

process_error(MysqlPid, State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = process_error,
		description = "mysqld process not found",
		data = [
			{process_id, MysqlPid},
			{pidfile_path, State#state.pidfile_path}
		]
	}.
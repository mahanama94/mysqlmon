%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements periodic check on the mysqld error log (mysqld.log) to Detect Errors
%%% Procedure
%%%     - Checking the mysqld.log file for changes. (Using file info)
%%%     - If changed, Read last lines for regex error
%%%     - Classify errors using regex mapping on each line
%%%
%%%
%%% App Env
%%% check_interval  : Interval for sampling. Default 1000ms.
%%% warning         : Alert for warning (true, false),
%%% file_path       : File path for the log file. Default "/var/log/"
%%% file_lines      : Number of lines from the tail to analyze. Default 10 lines.
%%%
%%% NOTES
%%% Run mode of the application requires the permission to view  the Log file. (Read permission)
%%%
%%% @end
%%% Created : 17. Nov 2017 3:07 PM
%%%-------------------------------------------------------------------
-module(mysqlmon_errorfile_server).
-author("bhanuka").

-include_lib("kernel/include/file.hrl").
-include("mysqlmon.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, get_error_type/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(SERVICE, ?MODULE).

-record(state, { check_interval, file_name, update_time, file_lines}).

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
	CheckInterval = proplists:get_value(check_interval, Args, 1000),
	FilePath = proplists:get_value(file_path, Args, "/var/log/"),
	FileName = lists:concat([FilePath, "mysqld.log"]),
	FileLines = proplists:get_value(file_lines, Args, 10),
	
	{ok, #state{check_interval = CheckInterval, file_name = FileName, file_lines = FileLines}, CheckInterval}.

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
handle_call(_Request, _From, State) ->
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
handle_cast(_Request, State) ->
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
	LastUpdateTime  = State#state.update_time,
	FileName        = State#state.file_name,
	CheckInterval   = State#state.check_interval,
	FileLines       = State#state.file_lines,
%%	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p checking for file : ~p ~n", [?MODULE, ?LINE, FileName]),
	
	NewState =
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			NewUpdateTime = FileInfo#file_info.mtime,
			case LastUpdateTime of
				undefined ->
					?LOGMSG(?APP_NAME, ?INFO, "~p | ~p LastUpdateTime undefined ~n", [?MODULE, ?LINE]),
					State#state{ update_time = NewUpdateTime};
				_UpdateTime ->
					if
						NewUpdateTime == LastUpdateTime ->
							?LOGMSG(?APP_NAME, ?INFO, "~p | ~p no change in update time ~n", [?MODULE, ?LINE]),
							State;
						true ->
							CmdData  = os:cmd(lists:concat(["tail -n ", FileLines, " ", FileName])),
							case check_errors(CmdData) of
								[] ->
									?LOGMSG(?APP_NAME, ?INFO, "~p | ~p NO errors found ~n", [?MODULE, ?LINE]),
									State#state{ update_time = NewUpdateTime};
								Errors ->
									?LOGMSG(?APP_NAME, ?INFO, "~p | ~p Errors found Errors : ~p ~n", [?MODULE, ?LINE, Errors]),
									mysqlmon_util:send_router(?SERVICE, Errors),
									State#state{ update_time = NewUpdateTime}
							end
					end
			end;
		{error, Reason} ->
			?LOGMSG(?APP_NAME, ?INFO, "~p | ~p read_file_info error Reason : ~p ~n", [?MODULE, ?LINE, Reason]),
			mysqlmon_util:send_router(?SERVICE, logfile_error(Reason, State)),
			State
	end,

	{noreply, NewState, CheckInterval};

handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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
-spec(check_errors(CmdData :: list()) -> list()).
check_errors(CmdData) ->
	Data = string:tokens(CmdData, "\n"),
	lists:foldl(fun(Line, Errors) -> case process_line(Line) of ok -> Errors; Error -> Errors ++ [Error]end end, [], Data).

process_line(Line) ->
	case re:run(Line, "error", [{capture, first, list}, caseless]) of
		nomatch ->
			ok;
		{match, _} ->
			classify_error(Line)
	end,
	ok.

classify_error(Line) ->
	ErrorType = get_error_type(Line),
	#mysqlmon_event{
		service = ?SERVICE,
		type = ErrorType,
		%% TODO - complete reason
		description = Line,
		data = [
			{error_log, Line}
		]
	}.

get_error_type(Line) ->
	%% error_type, regex
	%% TODO - needs to add as list, complete for single string
	Mappings = [
		{ndb_cluster_error, ["ndb", "cluster"]},
		{connection_error,  ["connection", "socket", "packets"]}
	],
	case get_error_type(Line, Mappings) of
		nomatch ->
			unknown_error;
		ErrorType ->
			ErrorType
	end.

get_error_type(Line, [Mapping| Rest]) ->
	case get_error_type(Line, Mapping) of
		nomatch ->
			get_error_type(Line, Rest);
		ErrorType ->
			ErrorType
	end;

get_error_type(_Line, []) ->
	nomatch;

get_error_type(Line, {ErrorType, RegexList}) ->
	ReOptions = [{capture, first, list}, caseless],
	case lists:foldl(fun(Regex, Acc) -> case re:run(Line, Regex, ReOptions) of nomatch -> Acc; _ -> true end end, false, RegexList) of
		true ->
			ErrorType;
		_ ->
			nomatch
	end.

logfile_error(Reason, State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = logfile_error,
		description = Reason,
		data = [
			{logfile, State#state.file_name}
		]
	}.
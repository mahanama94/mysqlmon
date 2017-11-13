%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements periodic check for cluster manager or ndb node pid file
%%%
%%% App env
%%% file_path   : Directory path for the pid file. Usually data directory of the node. Default "/var/lib/mysql-cluster"
%%% node_id     : Node id for the node. (See configuration of NDB nodes)
%%% node_type   : Type of Node. cm - cluster manager, ndb - data nodes. Default ndb
%%% check_interval : Period for checking. Default 1000ms.
%%%
%%% NOTES
%%% Run mode of the application requires the permission to view Pid file. (you may use sudo)
%%% May detect pid file missing if Node Id is not properly configured.
%%%
%%% @end
%%% Created : 10. Nov 2017 12:36 PM
%%%-------------------------------------------------------------------
-module(mysqlmon_ndbpidfile_server).
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

-record(state, {check_interval, pidfile_path, node_id, node_type}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link( Args ::term()) ->
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
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p Args : ~p ~n", [?MODULE, ?LINE, Args]),
	CheckInterval   = proplists:get_value(check_interval, Args, 1000),
	PidFilePath     = proplists:get_value(check_interval, Args, "/var/lib/mysql-cluster/"),
	NodeType        = proplists:get_value(node_type, Args, ndb),
	Node            = proplists:get_value(node_id, Args, 0),
	NodeId =
	case is_integer(Node) of
		true ->
			lists:concat(["", Node]);
		_ ->
			Node
	end,
	
	{ok, #state{check_interval = CheckInterval, pidfile_path = PidFilePath, node_id = NodeId, node_type = NodeType}, CheckInterval}.

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
%% TODO - send notification
handle_info(timeout, State) ->
	CheckInterval = State#state.check_interval,
	NodeType = State#state.node_type,
	
	PidFileName = lists:concat(["ndb_", State#state.node_id, ".pid"]),
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
							?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p ~p process doesn't exists Pid : ~p ~n", [?MODULE, ?LINE, NodeType, MysqlPid]);
						_ ->
							?LOGMSG(?APP_NAME,?INFO, "~p | ~p ~p process exists Pid : ~p ~n", [?MODULE, ?LINE, NodeType, MysqlPid])
					end ;
				{error, Reason} ->
					?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p Error Reading file Reason : ~p ~n", [?MODULE, ?LINE, Reason]);
				eof ->
					?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p File is empty ~n", [?MODULE, ?LINE])
			end,
			file:close(IoDevice);
		{error, enoent} ->
			?LOGMSG(?APP_NAME, ?INFO, "~p | ~p file not found Reason : ~p ~n", [?MODULE, ?LINE, enoent]);
		{error, eacces} ->
			?LOGMSG(?APP_NAME, ?INFO, "~p | ~p file access failed Reason : ~p ~n", [?MODULE, ?LINE, eaccess]);
		{error, Reason} ->
			?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql pidfile missing Reason : ~p Path ~p ~n", [?MODULE, ?LINE, Reason, FilePath])
	end,
	{noreply, State, CheckInterval};

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

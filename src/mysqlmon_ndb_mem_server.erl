%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements periodic check on memory usage of NDB Storage Engine
%%%
%%% App env
%%% crit_index  : Index memory threshold for index memory warning
%%% warn_index  : Index memory threshold for index memory critical
%%% crit_data   : Data memory threshold for data memory warning
%%% warn_data   : Data memory threshold for data memory critical
%%%
%%% NOTES
%%% Can be run on any Node in the cluster with ndb_mgm installed
%%% Raised alarm if ndb_mgm is unable to perform the action ( ex : When disconnected from cluster )
%%% Requires executing user level permission to run "ndb_mgm"
%%%
%%% @end
%%% Created : 10. Nov 2017 4:55 PM
%%%-------------------------------------------------------------------
-module(mysqlmon_ndb_mem_server).
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

-record(state, {warn_index, crit_index, warn_data, crit_data, check_interval}).

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
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p starting mysqlmon_ndb_mem_server Args : ~p ~n", [?MODULE, ?LINE, Args]),
	
	CheckInterval   = proplists:get_value(check_interval, Args, 1000),
	WarnIndex       = proplists:get_value(warn_index, Args, 80),
	CritIndex       = proplists:get_value(crit_index, Args, 90),
	WarnData        = proplists:get_value(warn_index, Args, 70),
	CritData        = proplists:get_value(crit_index, Args, 80),
	
	{ok, #state{check_interval = CheckInterval, warn_index = WarnIndex, crit_index = CritIndex, warn_data = WarnData,crit_data = CritData}, CheckInterval}.

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
	CheckInterval = State#state.check_interval,
	CmdData = os:cmd("ndb_mgm -e 'all report memory'"),
	case string:str(CmdData, "not found") of
		0 ->
			case string:str(CmdData, "Unable to connect") of
				0 ->
					%% process cmd data
					ok;
				_Other ->
					mysqlmon_util:send_router(?SERVICE, unable_to_connect)
			end;
		_Other ->
			mysqlmon_util:send_router(?SERVICE, command_not_founud)
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

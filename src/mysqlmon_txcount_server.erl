%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% Module implements periodic check on TPS of MySQL ( SQL Node)
%%%
%%% App env
%%% check_interval  : Interval for sampling. Defualt 1000ms
%%% warn_threshold  : TPS to generate warning. Default 100.
%%% crit_threshold  : TPS to generate critical state. Default 1000.
%%% dsn             : Dsn for the SQL Node
%%%
%%% NOTES
%%% SQL Node can be remotely monitored.
%%% Generate critical alarm if unable to connect to SQL Node.
%%% Requires SQL permission to perform SQL "SHOW GLOBAL STATUS"
%%% Change in Check Interval takes average of transactions of period.
%%%
%%% @end
%%% Created : 09. Nov 2017 12:00 PM
%%%-------------------------------------------------------------------
-module(mysqlmon_txcount_server).
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

-record(state, { check_interval, warn_threshold, crit_threshold, dsn, timestamp}).

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
	?LOGMSG(?APP_NAME, ?INFO, "~p |  ~p starting Args : ~p ~n", [?MODULE, ?LINE, Args]),
	WarnThreshold   = proplists:get_value(warn_threshold, Args, 100),
	CritThreshold   = proplists:get_value(crit_threshold, Args, 1000),
	CheckInterval   = proplists:get_value(check_interval, Args, 10000),
	Dsn             = proplists:get_value(dsn, Args, ""),
	TimeStamp       = calendar:local_time(),
	{ok, #state{warn_threshold = WarnThreshold, crit_threshold = CritThreshold, check_interval = CheckInterval, dsn = Dsn, timestamp = TimeStamp}, CheckInterval}.

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
  ?LOGMSG(?APP_NAME, ?WARNING, "~p | ~p unsupported handle_call Request : ~p From : ~p ~n", [?MODULE, ?LINE, Request, From]),
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
  ?LOGMSG(?APP_NAME, ?WARNING, "~p | ~p unsupported handle_call Request : ~p ~n", [?MODULE, ?LINE, Request]),
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
	Dsn = State#state.dsn,
	CritThreshold = State#state.crit_threshold,
	WarnThreshold = State#state.warn_threshold,
	NewState  =
	case odbc:connect(Dsn, []) of
		{ok, Ref} ->
			case odbc:sql_query(Ref, "SHOW STATUS WHERE variable_name = 'Threads_connected'") of
				{selected, _Columns, Rows} ->
					TransctionCount = get_transaction_count(Rows),
					LastTime = State#state.timestamp,
					NowTime = calendar:local_time(),
					{Hour, Minute, Seconds} = calendar:time_difference(LastTime, NowTime),
					Tps = TransctionCount / (Hour * 3600 + Minute * 60 + Seconds),
					if
						Tps > CritThreshold ->
							mysqlmon_util:send_router(?SERVICE, transactions_critical(Tps, State)),
							?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql TPS critical TPS : ~p  ~n", [?MODULE, ?LINE, Tps]);
						Tps > WarnThreshold ->
							mysqlmon_util:send_router(?SERVICE, transactions_warning(Tps, State)),
							?LOGMSG(?APP_NAME, ?WARNING,"~p | ~p mysql TPS warning TPS : ~p ~n", [?MODULE, ?LINE, Tps]);
						true ->
							?LOGMSG(?APP_NAME, ?INFO, "~p | ~p mysql TPS normal TPS : ~p  ~n",[?MODULE, ?LINE, Tps])
					end,
					State#state{ timestamp = NowTime};
				{error, Reason} ->
					mysqlmon_util:send_router(?SERVICE,query_error(Reason, State)),
					?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql query failed Reason : ~p ~n", [?MODULE, ?LINE, Reason]),
					State
			end;
		{error, Reason} ->
			mysqlmon_util:send_router(?SERVICE, odbc_error(Reason, State)),
			?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p mysql connection failed Reason : ~p ~n", [?MODULE, ?LINE, Reason]),
			State
	end,
	?LOGMSG(?APP_NAME, ?INFO,"~p | ~p handling timeout ~n", [?MODULE, ?LINE]),
	{noreply, NewState, CheckInterval};

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


get_transaction_count(List) ->
	get_transaction_count(List, 0).

get_transaction_count([Head | Tail], Count) ->
	{_Desc, TxCount} = Head,
	Count + TxCount + get_transaction_count(Tail, Count);

get_transaction_count([Head], Count) ->
	{_Desc, TxCount} = Head,
	Count + TxCount.

transactions_critical(Connections,State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = transactions_critical,
		description = "transactions count critical",
		data = [
			{transactions_count, Connections},
			{critical_threshold, State#state.crit_threshold}
		]
	}.

transactions_warning(Connections, State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = transactions_warning,
		description = "transactions count warning",
		data = [
			{transactions_count, Connections},
			{warning_threshold, State#state.warn_threshold}
		]
	}.

query_error(Reason, State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = query_error,
		description = "error executing query",
		data = [
			{reason, Reason},
			{dsn, State#state.dsn}
		]
	}.

odbc_error(Reason, State) ->
	#mysqlmon_event{
		service = ?SERVICE,
		type = odbc_error,
		description = "Odbc driver error",
		data = [
			{reason, Reason},
			{dsn, State#state.dsn}
		]
	}.
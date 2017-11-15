%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 10:32 AM
%%%-------------------------------------------------------------------
-module(mysqlmon_snmp_server).
-author("bhanuka").

-include("mysqlmon.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {subscriptions, status}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p starting mysqlmon_snmp_server Pid : ~p ~n", [?MODULE, ?LINE,  self()]),
%%	_Result = applications:start(snmp),
%%	MibName = application:get_env(mibname, mysqlmon, "MYSQLMON-MIB"),
%%	_MibResult =
%%	case snmps:whereis_mib(MibName) of
%%		{ok, _Path} ->
%%			ok;
%%		{error, not_found} ->
%%			PrivDir = code:priv_dir(?APP_NAME),
%%			?LOGMSG(?APP_NAME, ?INFO, "~p |~p PrivDir : ~p ~n", [?MODULE, ?LINE, PrivDir]),
%%			snmpa:load_mibs([PrivDir ++ "/mibs/" ++ MibName])
%%	end,
	Subscriptions = application:get_env(mysqlmon, services, []),
	{ok, #state{subscriptions = Subscriptions, status = not_subscribed}, 1000}.

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

handle_call(Request, _From, State) ->
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p unsupported handle_call Request : ~p ~n", [?MODULE, ?LINE, Request]),
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
	?LOGMSG(?APP_NAME, ?LINE, "~p | ~p unsupported handle_cast Rquest : ~p ~n", [?MODULE,?LINE, Request]),
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

handle_info(timeout, #state{ status = not_subscribed} = State) ->
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p handling timeout ~n", [?MODULE, ?LINE]),
	subscribe_services(State#state.subscriptions),
	{noreply, State#state{ status = subscribed}};
	
handle_info(Info, State) ->
	?LOGMSG(?APP_NAME, ?INFO, "~p | ~p unsupported handle info Info : ~p ~n", [?MODULE, ?LINE, Info]),
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
	?LOGMSG(?APP_NAME, ?ERROR, "~p | ~p terminating mysqlmon_snmp_server Reason : ~p ~n", [?MODULE, ?LINE, Reason]),
	%% TODO - delete subscriptions
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

subscribe_services(Services) ->
	lists:map(fun(Service) -> mysqlmon_util:subscribe_service(Service, self()) end, Services).
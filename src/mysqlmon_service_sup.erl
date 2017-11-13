%%%-------------------------------------------------------------------
%%% @author bhanuka
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2017 11:48 AM
%%%-------------------------------------------------------------------
-module(mysqlmon_service_sup).
-author("bhanuka").

-include("mysqlmon.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	
	Services = application:get_env(mysqlmon, services, []),
	ChildSpecs = get_childspecs(Services),

	{ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_childspecs([]) ->
	[];

get_childspecs([Service| Rest]) ->
	lists:concat([[get_childspecs(Service)] , get_childspecs(Rest)]);
	
get_childspecs([Service]) ->
	get_childspecs(Service);
	
get_childspecs(Service) ->
	Restart = permanent,
	Shutdown = 2000,
	Type = worker,
	
	AppConfig = application:get_env(mysqlmon, Service, []),
	{Service, {Service, start_link, [AppConfig]}, Restart,Shutdown, Type, [Service]}.
	
	
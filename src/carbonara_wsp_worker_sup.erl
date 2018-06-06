-module(carbonara_wsp_worker_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(IDLE_ACCEPTORS, 10).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

restart_strategy() ->
    {simple_one_for_one, 60, 3600}.

child_spec() -> {
    carbonara_wsp_worker,
    {carbonara_wsp_worker, start_link, []},
    transient,
    1000,
    worker,
    []
}.

init([]) ->
    {ok, {restart_strategy(), [child_spec()]}}.

start_child(Metric) ->
    supervisor:start_child(?MODULE, [Metric]).

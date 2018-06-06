-module(carbonara_wsp_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%% API
-export([start_link/0]).

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

init([]) ->
    Procs = [
    {carbonara_wsp_worker_sup,
     {carbonara_wsp_worker_sup, start_link,[]},
      permanent,
      3000,
      supervisor,
      [carbonara_wsp_worker_sup]
    },
    {carbonara_wsp,
     {carbonara_wsp, start_link,[]},
      permanent,
      3000,
      worker,
      [carbonara_wsp]
    }
    ],
    {ok, {{one_for_all, 10, 10}, Procs}}.

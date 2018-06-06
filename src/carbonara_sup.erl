%% @private
-module(carbonara_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [
    {carbonara_wsp_sup,
     {carbonara_wsp_sup, start_link,[]},
      permanent,
      3000,
      supervisor,
      [carbonara_wsp_sup]
    },
    {carbonara_tcp_sup,
     {carbonara_tcp_sup, start_link,[]},
      permanent,
      3000,
      supervisor,
      [carbonara_tcp_sup]
    },
    {carbonara_udp,
     {carbonara_udp, start_link,[]},
      permanent,
      3000,
      worker,
      [carbonara_udp]
    }
    ],
    {ok, {{one_for_one, 10, 10}, Procs}}.

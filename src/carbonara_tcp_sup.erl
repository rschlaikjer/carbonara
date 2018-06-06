-module(carbonara_tcp_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Port} = application:get_env(carbonara, listen_port),
    lager:info("Listening on TCP port ~p~n", [Port]),
    {ok, Socket} = gen_tcp:listen(
        Port,
        [
            binary,
            {reuseaddr, true},
            {active, true},
            {send_timeout_close, true}
        ]
    ),
    spawn_link(fun empty_listener/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{carbonara_tcp_conn,
            {carbonara_tcp_conn, start_link, [Socket]},
            temporary, 1000, worker, []}
          ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listener() ->
    [start_socket() || _ <- lists:seq(1, ?IDLE_ACCEPTORS)],
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

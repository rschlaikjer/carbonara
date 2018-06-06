-module(carbonara_wsp).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Supervisor callback
-export([start_link/0]).

%% Public api
-export([get_handler/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    process_ets
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_handler(Metric) ->
    gen_server:call(?MODULE, {get_handler, Metric}).

init([]) ->
    ProcessEts = ets:new(wsp_processes, [set]),
    {ok, #state{process_ets=ProcessEts}}.

handle_call({get_handler, Metric}, _From, State) ->
    {reply, get_wsp_handler(State, Metric), State};
handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:info("Unexpected cast ~p~n", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Unexpected info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal

get_wsp_handler(State, MetricName) ->
    case ets:lookup(State#state.process_ets, MetricName) of
        [] ->
            % No handler for this metric - need to create
            {ok, Pid} = carbonara_wsp_worker_sup:start_child(MetricName),
            ets:insert(State#state.process_ets, {MetricName, Pid}),
            {ok, Pid};
        [{_, Pid}|_] ->
            % Already exists
            {ok, Pid}
    end.

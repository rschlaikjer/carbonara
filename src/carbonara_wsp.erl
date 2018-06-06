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
    metric_ets,  % Pid -> Metric
    process_ets  % Metric -> Pid
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_handler(Metric) ->
    gen_server:call(?MODULE, {get_handler, Metric}).

init([]) ->
    MetricEts = ets:new(wsp_metrics, [set]),
    ProcessEts = ets:new(wsp_processes, [set]),
    {ok, #state{
        metric_ets=MetricEts,
        process_ets=ProcessEts
    }}.

handle_call({get_handler, Metric}, _From, State) ->
    {reply, get_wsp_handler(State, Metric), State};
handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:info("Unexpected cast ~p~n", [Request]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Get the metric name for this pid
    case ets:lookup(State#state.metric_ets, Pid) of
        [] ->
            lager:info("Unexpected DOWN status for pid ~p", [Pid]);
        [{_, MetricName}|_] ->
            % Delete the metric & pid mappings for the downed worker
            ets:delete(State#state.metric_ets, Pid),
            ets:delete(State#state.process_ets, MetricName)
    end,
    {noreply, State};
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
            monitor(process, Pid),
            ets:insert(State#state.metric_ets, {Pid, MetricName}),
            ets:insert(State#state.process_ets, {MetricName, Pid}),
            {ok, Pid};
        [{_, Pid}|_] ->
            % Already exists
            {ok, Pid}
    end.

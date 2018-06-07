-module(carbonara_wsp_worker).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Supervisor callback
-export([start_link/1]).

%% Metric ingest
-export([handle_metric/4]).

%% Metric fetching
-export([
    fetch_metrics/2, fetch_metrics/3,
    fetch_metrics_async/2, fetch_metrics_async/3
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    metric_name,
    wsp_file
}).

start_link(MetricName) when is_list(MetricName) ->
    start_link(list_to_binary(MetricName));
start_link(MetricName) when is_binary(MetricName) ->
    gen_server:start_link(?MODULE, [MetricName], []).

handle_metric(Pid, Metric, Value, Timestamp) when is_list(Metric) ->
    handle_metric(Pid, list_to_binary(Metric), Value, Timestamp);
handle_metric(Pid, Metric, Value, Timestamp) when is_binary(Metric) ->
    gen_server:cast(Pid, {metric, Metric, Value, Timestamp}).

fetch_metrics(Pid, Since) ->
    fetch_metrics(Pid, Since, os:system_time(seconds)).
fetch_metrics(Pid, Since, Until) ->
    gen_server:call(Pid, {fetch_metrics, Since, Until}).

fetch_metrics_async(Pid, Since) ->
    fetch_metrics_async(Pid, Since, os:system_time(seconds)).
fetch_metrics_async(Pid, Since, Until) ->
    gen_server:call(Pid, {fetch_metrics_async, Since, Until}).

init([MetricName]) ->
    {ok, WspFile} = open_whisper_file(MetricName),
    {ok, #state{
        metric_name=MetricName,
        wsp_file=WspFile
    }}.

handle_call({fetch_metrics, Since, Until}, _From, State) ->
    Resp = wsp:fetch(State#state.wsp_file, os:system_time(seconds), Since, Until),
    {reply, Resp, State};
handle_call({fetch_metrics_async, Since, Until}, From, State) ->
    Ref = make_ref(),
    gen_server:cast(self(), {fetch_metrics_async, From, Ref, Since, Until}),
    {reply, {ok, Ref}, State};
handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast({fetch_metrics_async, {From, _}, Ref, Since, Until}, State) ->
    handle_async_fetch(State, From, Ref, Since, Until),
    {noreply, State};
handle_cast({metric, M, V, T}, State) ->
    handle_metric_internal(State, M, V, T),
    {noreply, State};
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

open_whisper_file(MetricName) ->
    WspFile = filename_for_metric(MetricName),
    ok = filelib:ensure_dir(WspFile),
    case filelib:is_file(WspFile) of
        true ->
            wsp:open(WspFile);
        false ->
            {ok, StorageSchema} = carbonara_schema:archive_settings_for_metric(
                MetricName
            ),
            wsp:create(WspFile, StorageSchema)
    end.

filename_for_metric(MetricName) ->
    {ok, BasePath} = application:get_env(carbonara, whisper_directory),
    Parts = binary:split(MetricName, <<".">>, [global]),
    filename:join([BasePath|Parts]).

handle_metric_internal(State, Metric, Value, Timestamp) ->
    case State#state.metric_name =:= Metric of
        false ->
            lager:warning("WSP handler for metric ~s got unexpected datum ~p", [
                Metric, {Value, Timestamp}
            ]);
        true ->
            aggregate_metric(State, Value, Timestamp)
    end.


aggregate_metric(State, Value, Timestamp) when is_integer(Value) ->
    aggregate_metric(State, float(Value), Timestamp);
aggregate_metric(State, Value, Timestamp) when is_float(Value) andalso is_integer(Timestamp) ->
    case wsp:update(State#state.wsp_file, os:system_time(second), Value, Timestamp) of
        ok -> ok;
        {error, Reason} ->
            lager:warning(
                "Failed to write datum ~p: ~p",
                [{State#state.metric_name, Value}, Reason]
            )
    end.

handle_async_fetch(State, From, Ref, Since, Until) ->
    Stat = State#state.metric_name,
    {ok, Metrics} = wsp:fetch(
        State#state.wsp_file,
        os:system_time(seconds),
        Since,
        Until
    ),
    From ! {wsp_metrics, Ref, Stat, Metrics}.

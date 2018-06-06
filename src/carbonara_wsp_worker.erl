-module(carbonara_wsp_worker).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Supervisor callback
-export([start_link/1]).

%% Metric ingest
-export([handle_metric/4]).

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

init([MetricName]) ->
    {ok, WspFile} = open_whisper_file(MetricName),
    {ok, #state{
        metric_name=MetricName,
        wsp_file=WspFile
    }}.

handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

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
    {WspDir, WspFile} = filename_for_metric(MetricName),
    ok = filelib:ensure_dir(WspDir),
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
    RevParts = lists:reverse(Parts),
    FileName = hd(RevParts),
    DirParts = lists:reverse(tl(RevParts)),
    Dir = filename:join([BasePath|DirParts]),
    File = filename:join(Dir, FileName),
    {Dir, File}.

handle_metric_internal(State, Metric, Value, Timestamp) ->
    case State#state.metric_name =:= Metric of
        false ->
            lager:warning("WSP handler for metric ~s got unexpected datum ~p", [
                Metric, {Value, Timestamp}
            ]);
        true ->
            aggregate_metric(State, Value, Timestamp)
    end.


aggregate_metric(State, Value, Timestamp) ->
    % lager:info("[~p:~p] ~s: ~p", [self(), Timestamp, State#state.metric_name, Value]).
    ok.

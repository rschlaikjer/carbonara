-module(carbonara_wsp).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Supervisor callback
-export([start_link/0]).

%% Public api
-export([
    get_handler/1,
    find_metrics/1,
    metrics_for_query/1,
    parallel_fetch_stats/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    metric_ets,  % Pid -> Metric
    process_ets,  % Metric -> Pid
    all_metric_ets % Set of all metric names
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_handler(Metric) ->
    gen_server:call(?MODULE, {get_handler, Metric}).

find_metrics(Query) when is_binary(Query) ->
    gen_server:call(?MODULE, {find_metrics, Query}).

metrics_for_query(Query) when is_binary(Query) ->
    gen_server:call(?MODULE, {metrics_for_query, Query}).

init([]) ->
    MetricEts = ets:new(wsp_metrics, [set]),
    ProcessEts = ets:new(wsp_processes, [set]),
    AllMetricsEts = ets:new(all_metrics, [set]),
    {ok, #state{
        metric_ets=MetricEts,
        process_ets=ProcessEts,
        all_metric_ets=AllMetricsEts
    }}.

handle_call({metrics_for_query, Query}, _From, State) ->
    {reply, metrics_for_query_impl(State, Query), State};
handle_call({find_metrics, Query}, _From, State) ->
    {reply, find_metrics_impl(State, Query), State};
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
            store_metric_name(State, MetricName),
            {ok, Pid};
        [{_, Pid}|_] ->
            % Already exists
            {ok, Pid}
    end.

store_metric_name(State, Metric) ->
    % Split on dots
    Parts = binary:split(Metric, <<".">>, [global]),
    ets:insert(State#state.all_metric_ets, {Metric, Parts}).

convert_regex(R) ->
    binary:replace(R, <<"*">>, <<"\\w*">>, [global]).

metric_names_for_query_list(State, RegexList) ->
    ets:foldl(
        fun({Metric, Parts}, Acc) ->
            case is_regex_prefix(RegexList, Parts) of
                false -> Acc;
                true -> [Metric|Acc]
            end
        end,
        [],
        State#state.all_metric_ets
    ).

metrics_for_query_impl(State, Query) when is_binary(Query) ->
    RegexList = binary:split(Query, <<".">>, [global]),
    Names = ets:foldl(
        fun({Metric, Parts}, Acc) ->
            case is_regex_match(RegexList, Parts) of
                false -> Acc;
                true -> [Metric|Acc]
            end
        end,
        [],
        State#state.all_metric_ets
    ),
    {ok, Names}.

metric_lists_for_query_list(State, RegexList) ->
    ets:foldl(
        fun({_Metric, Parts}, Acc) ->
            case is_regex_prefix(RegexList, Parts) of
                false -> Acc;
                true -> [Parts|Acc]
            end
        end,
        [],
        State#state.all_metric_ets
    ).

find_metrics_impl(State, Query) when is_binary(Query) ->
    RegexList = binary:split(Query, <<".">>, [global]),
    Metrics = metric_lists_for_query_list(State, RegexList),
    Records = [
        record_for_query(RegexList, Metric) || Metric <- Metrics
    ],
    % A bit lazy
    Records1 = sets:to_list(sets:from_list(Records)),
    {ok, Records1}.

record_for_query(Query, Metric) ->
    % The query is known to be a prefix of metric, so
    % drop as many fragments from the front of metric as there are in
    % the query
    {Query1, _} = lists:split(length(Query) - 1, Query),
    {MetricHead, MetricTail} = lists:split(length(Query1), Metric),

    % The 'text' is the firstmost item in the tail
    Text = hd(MetricTail),
    % The record is expandable if there exist more segments in the tail
    Expandable = length(MetricTail) > 1,
    % It's a leaf if it's not expandable
    Leaf = not Expandable,
    % ID is the query joined with the tail
    IdPrefix = << <<A/binary, ".">> || A <- Query1 >>,
    Id = <<IdPrefix/binary, Text/binary>>,
    % Not sure how these two are different
    AllowChildren = Expandable,

    % Proplist it up
    [
        {text, Text},
        {expandable, Expandable},
        {leaf, Leaf},
        {id, Id},
        {allowChildren, AllowChildren}
    ].

is_regex_match([], []) -> true; % Fully equal
is_regex_match([], _) -> false;  % Unmatched
is_regex_match(_, []) -> false; % Unmatches
is_regex_match([H|AT], [H|BT]) -> is_regex_match(AT, BT);
is_regex_match([Rex|AT], [H|BT]) ->
    case re:run(H, convert_regex(Rex)) of
        nomatch -> false;
        _ -> is_regex_match(AT, BT)
    end.

is_regex_prefix([], []) -> true; % Fully equal
is_regex_prefix([], _) -> true;  % Regex prefix is shorter
is_regex_prefix(_, []) -> false; % Regex prefix is longer
is_regex_prefix([H|AT], [H|BT]) -> is_regex_prefix(AT, BT);
is_regex_prefix([Rex|AT], [H|BT]) ->
    case re:run(H, convert_regex(Rex)) of
        nomatch -> false;
        _ -> is_regex_prefix(AT, BT)
    end.

parallel_fetch_stats(Stats, Since) ->
    % Request stats from each WSP handler
    {Refs, Pids} = lists:foldl(
        fun(Stat, {Refs, Pids}) ->
            {ok, Pid} = carbonara_wsp:get_handler(Stat),
            erlang:monitor(process, Pid),
            {ok, Ref} = carbonara_wsp_worker:fetch_metrics_async(Pid, Since),
            _Acc1 = {
                maps:put(Ref, Stat, Refs),
                maps:put(Pid, Ref, Pids)
            }
        end,
        {#{}, #{}},
        Stats
    ),
    await_parallel_stats(Refs, Pids, []).

await_parallel_stats(Refs, Pids, Acc) ->
    case maps:size(Refs) of
        0 -> {ok, Acc};
        _ ->
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    % Whisper handler went down - we will never hear back
                    % Remove the ref, we will just not return data for them.
                    Ref = maps:get(Pid, Pids),
                    Refs1 = maps:remove(Ref, Refs),
                    await_parallel_stats(Refs1, Pids, Acc);
                {wsp_metrics, Ref, Stat, Metrics} ->
                    Refs1 = maps:remove(Ref, Refs),
                    await_parallel_stats(Refs1, Pids, [{Stat, Metrics}|Acc])
            end
    end.

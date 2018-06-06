-module(carbonara_schema).
-export([
    archive_settings_for_metric/1
]).

retention_seconds(I) when is_integer(I) -> I;
retention_seconds({Count, second}) -> Count;
retention_seconds({Count, minute}) -> retention_seconds({60 * Count, second});
retention_seconds({Count, hour}) -> retention_seconds({60 * Count, minute});
retention_seconds({Count, day}) -> retention_seconds({24 * Count, hour});
retention_seconds({Count, year}) -> retention_seconds({365 * Count, day}).

archive_settings_for_storage_schema(Schema) ->
    % Schema must be a list of tuples of time units
    % Time units may be integer seconds, or 2-tuples of {count, unit}
    % where unit is on of [second, minute, hour, day, year]
    % Returns a proplist of {metric_duration, bucketing}
    % with both in seconds
    [{retention_seconds(Duration), retention_seconds(Bucketing)}
     || {Duration, Bucketing} <- Schema].

retention_policies() ->
    {ok, Schemas} = application:get_env(carbonara, storage_schemas),
    Schemas.

archive_settings_for_metric(MetricName) when is_binary(MetricName) ->
    archive_settings_for_metric(MetricName, retention_policies()).

archive_settings_for_metric(MetricName, []) ->
    % No applicable policy found!
    lager:error("No applicable storage schema for metric ~p", [MetricName]),
    {error, not_found};
archive_settings_for_metric(MetricName, [Schema|Schemas]) ->
    {_SchemaName, SchemaSettings} = Schema,
    {pattern, Pattern} = proplists:lookup(pattern, SchemaSettings),
    case pattern_matches_metric(MetricName, Pattern) of
        false ->
            archive_settings_for_metric(MetricName, Schemas);
        true ->
            {retentions, Retentions} = proplists:lookup(retentions, SchemaSettings),
            {ok, archive_settings_for_storage_schema(Retentions)}
    end.

pattern_matches_metric(_Metric, any) -> true;
pattern_matches_metric(Metric, Pattern) when is_binary(Pattern) ->
    case re:run(Metric, Pattern) of
        nomatch -> false;
        _ -> true
    end.

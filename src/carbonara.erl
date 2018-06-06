-module(carbonara).
-compile([{parse_transform, lager_transform}]).
-compile(export_all).

parse_float(F) ->
    try erlang:binary_to_float(F)
    catch error:badarg -> erlang:binary_to_integer(F)
    end.

line_to_stat(Line) ->
    case binary:split(Line, <<" ">>, [global]) of
        [Metric, ValueBin, TimestampBin] ->
            Timestamp = binary_to_integer(TimestampBin),
            Value = parse_float(ValueBin),
            {ok, Metric, Value, Timestamp};
        _ ->
            {error, Line}
    end.

handle_stat({error, Line}) ->
    lager:error("Bad metric line: ~p~n", [Line]);
handle_stat({ok, Metric, Value, Timestamp}) ->
    case carbonara_wsp:get_handler(Metric) of
        {ok, Pid} ->
            carbonara_wsp_worker:handle_metric(Pid, Metric, Value, Timestamp)
    end.

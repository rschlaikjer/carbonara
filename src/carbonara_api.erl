-module(carbonara_api).
-compile([{parse_transform, lager_transform}]).

-export([init/0]).

-export([
    init/3,
    handle/2,
    terminate/3
]).

init() ->
    {ok, Port} = application:get_env(carbonara, api_port),
    Dispatch = cowboy_router:compile([{'_', [{'_', ?MODULE, []}]}]),
    {ok, _} = cowboy:start_http(
        http, 10, [{port, Port}], [{env, [{dispatch, Dispatch}]}]
    ),
    ok.

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    {ok, Req2} = handle_path(Req1, Path),
    {ok, Req2, State}.

handle_path(Req, <<"/render">>) ->
    render(Req);
handle_path(Req, <<"/metrics/find">>) ->
    find_metrics(Req);
handle_path(Req, Path) ->
    lager:info("Unexpected path: ~p~n", [Path]),
    write_reply(Req, <<"not found">>, 404).

internal_error(Req) ->
    write_reply(Req, <<"">>, 500).

write_reply(Req, Data) ->
    write_reply(Req, Data, 200).
write_reply(Req, Data, Code) ->
    cowboy_req:reply(
      Code,
      [{<<"content-type">>, <<"application/octet-stream">>}],
      Data,
      Req
     ).

terminate(_Reason, _Req, _State) ->
    ok.

find_metrics(Req1) ->
    {QueryParams, Req2} = cowboy_req:qs_vals(Req1),
    Query = proplists:get_value(<<"query">>, QueryParams, <<"*">>),
    {ok, Metrics} = carbonara_wsp:find_metrics(Query),
    Json = jsx:encode(Metrics),
    write_reply(Req2, Json).

render(Req1) ->
    % Return a list of [{target, MetricName}, {datapoints, [[value, time]...]}]
    {ok, BodyParams, Req2} = cowboy_req:body_qs(Req1),
    Target = proplists:get_value(<<"target">>, BodyParams),
    From = proplists:get_value(<<"from">>, BodyParams),
    Until = proplists:get_value(<<"until">>, BodyParams),
    Format = proplists:get_value(<<"format">>, BodyParams),
    MaxDataPoints = proplists:get_value(<<"maxDataPoints">>, BodyParams),

    {ok, Metrics} = carbonara_wsp:metrics_for_query(Target),
    {ok, Stats} = carbonara_wsp:parallel_fetch_stats(Metrics, 0),
    Response = [
        [{target, Stat}, {datapoints, [[V, T] || {V, T} <- Data]}] || {Stat, Data} <- Stats
    ],
    Json = jsx:encode(Response),
    write_reply(Req2, Json).

-module(carbonara_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    carbonara_api:init(),
    carbonara_sup:start_link().

stop(State) ->
    ok.

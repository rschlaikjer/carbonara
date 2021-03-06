-module(wsp).
-export([
    open/1,
    get_storage_schema/1,
    create/2,
    update/4,
    fetch/3, fetch/4
]).

-on_load(init/0).

-define(APP_NAME, carbonara).
-define(LIB_NAME, wsp).

get_storage_schema(_) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

open(_) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

create(_, _) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

update(_Fd, _NowSecs, _Value, _Timestamp) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

fetch(_Fd, _NowSecs, _From) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

fetch(_Fd, _NowSecs, _From, _Until) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

init() ->
    SoName = case code:priv_dir(?APP_NAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIB_NAME]);
                _ ->
                    filename:join([priv, ?LIB_NAME])
            end;
        Dir ->
            filename:join(Dir, ?LIB_NAME)
    end,
    erlang:load_nif(SoName, 0).

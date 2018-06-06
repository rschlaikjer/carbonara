-module(carbonara_udp).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% Supervisor callback
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, Port} = application:get_env(carbonara, listen_port),
    lager:info("Listening on UDP port ~p~n", [Port]),
    {ok, Socket} = gen_udp:open(
        Port,
        [
            binary,
            {reuseaddr, true},
            {active, true}
        ]
    ),
    {ok, #state{socket=Socket}}.

handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:info("Unexpected cast ~p~n", [Request]),
    {noreply, State}.

handle_info({udp, S, _SrcIp, _SrcPort, Data}, State=#state{socket=S}) ->
    handle_datagram(Data),
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

handle_datagram(<<>>) -> ok;
handle_datagram(<<Data/binary>>) ->
    case binary:split(Data, <<"\n">>) of
        [Line|[]] ->
            Stat = carbonara:line_to_stat(Line),
            carbonara:handle_stat(Stat);
        [Line|Lines] ->
            Stat = carbonara:line_to_stat(Line),
            carbonara:handle_stat(Stat),
            handle_datagram(hd(Lines))
    end.

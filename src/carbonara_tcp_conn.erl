-module(carbonara_tcp_conn).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    socket,
    buffer = <<>>
}).

%% Public API

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% Callbacks

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{
        socket = Socket
    }}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(accept, State=#state{socket=S}) ->
    % Accept the connection
    % This will block this process until a client connects
    AcceptResult = gen_tcp:accept(S),

    % Now that accept has finished, we've been engaged - tell the supervisor
    % to start a new acceptor to replace us
    carbonara_tcp_sup:start_socket(),

    % Now that we've replaced ourselves, assert the accept was a success
    case AcceptResult of
        {error, Error} ->
            lager:error("Failed to accept TCP connection: ~p~n", [Error]),
            {stop, normal, State};
        {ok, AcceptSocket} ->
            %% Replace our listen socket with the accepted socket
            {noreply, State#state{socket=AcceptSocket}}
    end;
handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info({tcp, S, Data}, State=#state{socket=S}) ->
    State1 = handle_datagram(State, Data),
    {noreply, State1};
handle_info({tcp_closed, S}, State=#state{socket=S}) ->
    {stop, normal, State};
handle_info({tcp_error, S, Reason}, State=#state{socket=S}) ->
    lager:error("TCP connection error: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Implementation

handle_datagram(State=#state{buffer=Buf}, Data) ->
    Buffer1 = consume_buffer(<<Buf/binary, Data/binary>>),
    State#state{buffer=Buffer1}.

consume_buffer(<<>>) -> ok;
consume_buffer(<<Data/binary>>) ->
    case binary:split(Data, <<"\n">>) of
        [Data] ->
            % Incomplete data
            Data;
        [Line|[]] ->
            Stat = carbonara:line_to_stat(Line),
            carbonara:handle_stat(Stat);
        [Line|Lines] ->
            Stat = carbonara:line_to_stat(Line),
            carbonara:handle_stat(Stat),
            consume_buffer(hd(Lines))
    end.

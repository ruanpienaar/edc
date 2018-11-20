-module(edc_tcpv4).

-export([
    start_link/1,
    send/2,
    send/3,
    simple_send/3
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc simple tries to conenct and send, on each call.
%%      connected keeps the connection open, with heartbeats checking for dead peers.
%% @end

%% ---------------------------------------------------------------------------------------

% -spec start_link(proplists:proplist(), proplists:proplist()) -> {ok, pid()}.
start_link({ConnectOpts, SocketOpts}) ->
    gen_server:start_link(?MODULE, {ConnectOpts, SocketOpts}, []).

send(Pid, Data) ->
    send(Pid, Data, 1000).

send(Pid, Data, Timeout) ->
    gen_server:call(Pid, {send, Data}, Timeout).

simple_send(Data, ConnectOpts, SocketOpts) ->
    case connect(ConnectOpts, SocketOpts) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    gen_tcp:close(Socket),
                    ok;
                {error, SendReason} ->
                    gen_tcp:close(Socket),
                    {error, SendReason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% i know - generating atoms is not ideal....
% name(Host, Port) ->
%     list_to_atom("edc_tcpv4_"++list_to_atom(Host)++"_"++integer_to_list(Port)).

%% ---------------------------------------------------------------------------------------
init({ConnectOpts, SocketOpts}) ->
    {host, Host} = lists:keyfind(host, 1, ConnectOpts),
    {port, Port} = lists:keyfind(port, 1, ConnectOpts),
    Ref = erlang:make_ref(),
    true = ets:insert(connection_table, _Entry={_Key={tcpv4, Host, Port, Ref}, self()}),
    Socket = 
        case connect(ConnectOpts, SocketOpts) of
            {ok, NewSocket} ->
                NewSocket;
            {error, Reason} ->
                % TODO: log the reason
                % TODO: do we remove a client that cannot connect ?
                %       or just simply increase the waiting time ?
                %       or try to re-connect once data needs to be sent ?
                undefined
        end,
    {ok, #{
        socket => Socket,
        connect_opts => ConnectOpts,
        socket_opts => SocketOpts
    }}.

% send - but socket failed - so reconnect
handle_call({send, Data}, _From, 
        #{ socket := undefined,
           connect_opts := ConnectOpts,
           socket_opts := SocketOpts } = State) ->
    edc_log:log(debug, "[~p] ~p send ~p\n", [?MODULE, self(), Data]),
    % timer:sleep(5000),
    {Reply, Socket} = case connect(ConnectOpts, SocketOpts) of
        {ok, NewSocket} ->
            ok = gen_tcp:send(NewSocket, Data),
            {ok, NewSocket};
        {error, Reason} ->
            {{error, Reason}, undefined}
    end,
    {reply, Reply, State#{ socket => Socket }};
% send - socket fine - send will check socket state
handle_call({send, Data}, _From, 
        #{ socket := Socket } = State) ->
    edc_log:log(debug, "[~p] ~p send ~p\n", [?MODULE, self(), Data]),
    % timer:sleep(5000),
    {Reply, NewSocket} = case gen_tcp:send(Socket, Data) of
        ok ->
            {ok, Socket};
        {error, Reason} ->
            {{error, Reason}, Socket}
    end,
    {reply, Reply, State#{ socket => NewSocket }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, #{ socket := Socket } = State) ->
    {noreply, State#{ socket := undefined }};
handle_info(Info, State) ->
    edc_log:log(warning, "[~p] handle_info ~p state ~p\n", [?MODULE, Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(ConnectOpts, SocketOpts) ->
    {host, Host} = lists:keyfind(host, 1, ConnectOpts),
    {port, Port} = lists:keyfind(port, 1, ConnectOpts),
    case lists:keyfind(timeout, 1, ConnectOpts) of
        false ->
            gen_tcp:connect(Host, Port, SocketOpts);
        {timeout, Timeout} ->
            gen_tcp:connect(Host, Port, SocketOpts, Timeout)
    end.
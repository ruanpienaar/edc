-module(edc_tcpv4).

-export([
    start_link/2
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @doc simple tries to conenct and send, on each call.
%%      connected keeps the connection open, with heartbeats checking for dead peers.
%% @end

-spec start_link(simple | connected, list()) -> {ok, pid()}.
start_link(Type, Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Type, Opts}, []).

init({simple, Opts}) ->
    {ok, #{
        opts => Opts
    }};
init({connected, Opts}) ->
    {ok, #{
        opts => Opts
    }}.

handle_call({send, Data}, _From, #{ opts := Opts } = State) ->
    {host, Host} = lists:keyfind(host, 1, Opts),
    {port, Port} = lists:keyfind(port, 1, Opts),
    {opts, SocketOpts} = lists:keyfind(opts, 1, Opts),
    ConnectFun = fun() ->
        case lists:keyfind(timeout, 1, Opts) of
            false ->
                gen_tcp:connect(Host, Port, SocketOpts);
            {timeout, Timeout} ->
                gen_tcp:connect(Host, Port, SocketOpts, Timeout)
        end
    end,
    Reply = case ConnectFun() of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Data),
            timer:sleep(4000),
            ok = gen_tcp:close(Socket),
            % SendResponse;
            ok;
        {error, Reason} ->
            {error, Reason}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

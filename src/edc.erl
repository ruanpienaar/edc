-module(edc).

-export([
    send/4,
    send/5,
    simple_send/4
]).

%% @doc send will use a socket from a pool, and send data through a client
%% @end

send(Host, Port, Data, Type = tcpv4) ->
    send(Host, Port, Data, Type = tcpv4, 5000).

send(Host, Port, Data, Type = tcpv4, Timeout) ->
    spawn_link(fun() ->
        poolboy:transaction(edc_sup:pool_name(Host, Port, Type), fun(Worker) ->
            gen_server:call(Worker, {send, Data}, Timeout)
        end)
    end).

simple_send(Host, Port, Data, Type = tcpv4) ->
    SocketOpts = [
        {packet, 2},
        {delay_send, false},
        {keepalive, true},
        {linger, {true, 1}},
        {show_econnreset, true},
        {active, once}
    ],
    simple_send(Host, Port, Data, Type, SocketOpts).

simple_send(Host, Port, Data, _Type = tcpv4, SocketOpts) ->
    edc_tcpv4:simple_send(Data, [{host, Host}, {port, Port}], SocketOpts).
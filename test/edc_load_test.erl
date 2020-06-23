-module(edc_load_test).

%% @doc send infinity/X amount of data from client to server.
%%      can specify inifinity for forever, and run/2 for the data to be sent
%% @end

-export([
    run/1,
    run/2
]).

run(X) ->
    B = <<0:65536>>,
    run(X, B).

run(infinity, B) ->
    do_send(B),
    run(infinity, B);
run(Times, B) when Times =< 0 ->
    ok;
run(Times, B) when Times > 0 ->
    do_send(B),
    run(Times-1, B).

do_send(B) ->
    random_sleep(),
    case edc:send("localhost", 7700, B, tcpv4) of
        ok ->
            ok;
        Error ->
            edc_log:log(warning, "[~p] do_send error ~p\n", [?MODULE, Error])
    end.

random_sleep() ->
    timer:sleep(round(rand:uniform() * 1000)).
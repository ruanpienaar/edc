-module(edc_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

-include("edc.hrl").

start(_StartType, _StartArgs) ->
    ets:new(connection_table, [named_table, public]),
    edc_sup:start_link().

stop(_State) ->
    ok.

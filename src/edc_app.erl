-module(edc_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include("edc.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

% from start-dev.sh
start() ->
    application:start(edc).

start(_StartType, _StartArgs) ->
	ets:new(connection_table, [named_table, public]),
    edc_sup:start_link().

stop(_State) ->
    ok.

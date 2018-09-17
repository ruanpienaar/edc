-module(edc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("edc.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    edc_sup:start_link().

stop(_State) ->
    ok.

-module(edc_log).

-export([
    log/3
]).

log(warning, FmtString, Args) ->
    io:format(FmtString, Args);
log(debug, FmtString, Args) ->
    case application:get_env(edc, log_debug, false) of
        true ->
            io:format(FmtString, Args);
        false ->
            ok
    end.
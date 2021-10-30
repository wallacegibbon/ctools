-module(cToolUtil).

-export([flatFmt/2]).

-spec flatFmt(string(), [any()]) -> string().
flatFmt(FmtString, Arguments) ->
    lists:flatten(io_lib:format(FmtString, Arguments)).

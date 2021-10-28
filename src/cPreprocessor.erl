-module(cPreprocessor).

-include("./eScanner.hrl").

-export([process/1]).

-spec process([token()]) -> {ok, [token()]} | {error, lineNumber(), string()}.
process(Tokens) ->
    {ok, []}.

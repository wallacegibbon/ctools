-module(cParser).

-include("./eScanner.hrl").

-export([parse/1]).

-type ast() :: any().

-spec parse([token()]) -> {ok, [ast()]} | {error, lineNumber(), string()}.
parse(Tokens) ->
    {ok, []}.

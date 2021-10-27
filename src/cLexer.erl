-module(cLexer).

-export([tokenize/1]).

-record(cIdentifier, {line = 0 :: integer(),
                      column = 0 :: integer(),
                      name :: string()}).

-record(cString, {line = 0 :: integer(),
                  column = 0 :: integer(),
                  value :: string()}).

-record(cInteger, {line = 0 :: integer(),
                   column = 0 :: integer(),
                   value :: integer()}).

-record(cCharacter, {line = 0 :: integer(),
                     column = 0 :: integer(),
                     value :: integer()}).

-record(cFloat, {line = 0 :: integer(),
                 column = 0 :: integer(),
                 value :: float()}).

-record(cOperator, {line = 0 :: integer(),
                    column = 0 :: integer(),
                    tag :: atom()}).

-type token() :: #cString{} | #cInteger{} | #cCharacter{} | #cFloat{} | #cIdentifier{} | #cOperator{} | endToken.

-spec getTokens(binary(), {integer(), integer(), [token()]}) -> [token()].
getTokens(<<${, Rest/binary>>, {CurrentLine, CurrentColumn, Tokens}) ->
    getTokens(Rest, {CurrentLine, CurrentColumn + 1, [#cOperator{line = CurrentLine, column = CurrentColumn, tag = '{'} | Tokens]});
getTokens(<<$}, Rest/binary>>, {CurrentLine, CurrentColumn, Tokens}) ->
    getTokens(Rest, {CurrentLine, CurrentColumn + 1, [#cOperator{line = CurrentLine, column = CurrentColumn, tag = '}'} | Tokens]});
getTokens(<<$\n, Rest/binary>>, {CurrentLine, CurrentColumn, Tokens}) ->
    getTokens(Rest, {CurrentLine + 1, 0, [#cOperator{line = CurrentLine, column = CurrentColumn, tag = newLine} | Tokens]});
getTokens(<<>>, {_, _, Tokens}) ->
    Tokens.

-spec tokenize(binary()) -> [token()].
tokenize(BinaryString) ->
    [].

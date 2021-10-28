-module(cScanner).

-export([tokenize/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type lineNumber() :: integer().
-type cOperator() :: {Tag :: atom(), lineNumber()}.
-type cFloat() :: {float, lineNumber(), Value :: float()}.
-type cInteger() :: {integer, lineNumber(), Value :: integer()}.
-type cCharacter() :: {character, lineNumber(), Value :: integer()}.
-type cString() :: {string, lineNumber(), Value :: string()}.
-type cIdentifier() :: {identifier, lineNumber(), Value :: atom()}.
-type endToken() :: endToken.

-type token() :: cOperator() | cFloat() | cInteger() | cCharacter() | cString() | cIdentifier() | endToken().
-type tokenContext() :: {lineNumber(), [token()]}.

-spec getTokens(binary(), tokenContext()) -> [token()].
getTokens(<<Character, Rest/binary>> = InputData, Context) ->
    case isElementOf(Character, spaceCharactersWithoutNewline()) of
        true ->
            getTokens(Rest, Context);
        false ->
            case isElementOf(Character, singleCharacterOperators()) of
                true ->
                    getSingleCharacterToken(InputData, Context);
                false ->
                    case isElementOf(Character, multiCharacterOperators()) of
                        true ->
                            getMultiCharacterToken(InputData, Context);
                        false ->
                            getMiscellaneousToken(InputData, Context)
                    end
            end
    end;
getTokens(<<>>, {_, Tokens}) ->
    lists:reverse(Tokens).

-spec isElementOf(Element, [Element]) -> boolean() when Element :: any().
isElementOf(Element, List) ->
    lists:any(fun (E) -> E =:= Element end, List).

-spec getMultiCharacterToken(binary(), tokenContext()) -> [token()].
getMultiCharacterToken(<<$>, $=, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'>=', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$>, $>, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'>>', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$>, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'>', CurrentLine} | Tokens]});
%%
getMultiCharacterToken(<<$<, $=, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'<=', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$<, $<, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'<<', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$<, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'<', CurrentLine} | Tokens]});
%%
getMultiCharacterToken(<<$+, $+, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'++', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$+, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'+', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$-, $-, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'--', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$-, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'-', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$&, $&, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'&&', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$&, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'&', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$|, $|, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'||', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$|, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'|', CurrentLine} | Tokens]});
getMultiCharacterToken(_, _) ->
    toRemove.

-spec getSingleCharacterToken(binary(), tokenContext()) -> [token()].
getSingleCharacterToken(<<Character, Rest/binary>>, {CurrentLine, Tokens}) ->
    case Character of
        $\n ->
            getTokens(Rest, {CurrentLine + 1, [{newline, CurrentLine} | Tokens]});
        _ ->
            getTokens(Rest, {CurrentLine, [{list_to_atom([Character]), CurrentLine} | Tokens]})
    end.

-spec getMiscellaneousToken(binary(), tokenContext()) -> [token()].
getMiscellaneousToken(<<$', Rest/binary>>, {CurrentLine, Tokens}) ->
    {CCharacter, RestContent} = getCharacterContent(Rest, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CCharacter | Tokens]});
getMiscellaneousToken(<<$", Rest/binary>>, {CurrentLine, Tokens}) ->
    {CString, RestContent, NewLineNumber} = getStringContent(Rest, CurrentLine),
    getTokens(RestContent, {NewLineNumber, [CString | Tokens]});
getMiscellaneousToken(Content, {CurrentLine, Tokens}) ->
    {CIdentifier, RestContent} = getIdentifier(Content, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CIdentifier | Tokens]}).

-spec getIdentifier(binary(), lineNumber()) -> {cIdentifier(), binary()}.
getIdentifier(<<Character, Rest/binary>>, CurrentLine)
        when Character >= $a, Character =< $z; Character >= $A, Character =< $Z; Character =:= $_ ->
    {IdentifierCharacters, RestContent} = getIdentifierCharacters(Rest, []),
    {{identifier, CurrentLine, [Character | IdentifierCharacters]}, RestContent};
getIdentifier(_, CurrentLine) ->
    throw({CurrentLine, "invalid identifier"}).

-spec getIdentifierCharacters(binary(), [integer()]) -> {string(), binary()}.
getIdentifierCharacters(<<Character, Rest/binary>>, CharacterCollect)
        when Character >= $a, Character =< $z; Character >= $A, Character =< $Z; Character =:= $_; Character >= $0, Character =< $9 ->
    getIdentifierCharacters(Rest, [Character | CharacterCollect]);
getIdentifierCharacters(RestContent, CharacterCollect) ->
    {lists:reverse(CharacterCollect), RestContent}.

-spec getStringContent(binary(), lineNumber()) -> {cString(), binary(), lineNumber()}.
getStringContent(Content, CurrentLine) ->
    getStringContent(Content, [], CurrentLine).

-spec getStringContent(binary(), [integer()], lineNumber()) -> {cString(), binary(), lineNumber()}.
getStringContent(<<$", Rest/binary>>, CharacterCollect, CurrentLine) ->
    {{string, CurrentLine, lists:reverse(CharacterCollect)}, Rest, CurrentLine};
getStringContent(<<$\\, $\n, Rest/binary>>, CharacterCollect, CurrentLine) ->
    getStringContent(Rest, CharacterCollect, CurrentLine + 1);
getStringContent(<<$\\, Rest/binary>>, CharacterCollect, CurrentLine) ->
    {Value, RestContent} = getEscapedCharacter(Rest, CurrentLine),
    getStringContent(RestContent, [Value | CharacterCollect], CurrentLine);
getStringContent(<<Character, Rest/binary>>, CharacterCollect, CurrentLine) ->
    getStringContent(Rest, [Character | CharacterCollect], CurrentLine).

-spec getCharacterContent(binary(), lineNumber()) -> {cCharacter(), binary()}.
getCharacterContent(<<$\\, Rest/binary>>, CurrentLine) ->
    {Value, RestContent} = getEscapedCharacter(Rest, CurrentLine),
    case RestContent of
        <<$', RealRestContent>> ->
            {{character, CurrentLine, Value}, RealRestContent};
        _ ->
            throw({CurrentLine, "missing \"'\" at the end of character"})
    end;
getCharacterContent(<<Character, $', Rest/binary>>, CurrentLine) ->
    {{character, CurrentLine, Character}, Rest};
getCharacterContent(<<Character, _/binary>>, CurrentLine) when Character =:= $\n; Character =:= $\t ->
    throw({CurrentLine, "invalid character literal"});
getCharacterContent(_, CurrentLine) ->
    throw({CurrentLine, "missing \"'\" at the end of character"}).

%% For now, hex and octal only support fixed format: \xab\xab, \0abc\0abc. I am not sure whether this follows
%% the C standard, so this function will be changed in the future when necessary.
-spec getEscapedCharacter(binary(), lineNumber()) -> {integer(), binary()}.
getEscapedCharacter(<<C, A, B, Rest/binary>>, LineNumber) when C =:= $x; C =:= $X ->
    FixedA = getHexDigit(A, LineNumber),
    FixedB = getHexDigit(B, LineNumber),
    {FixedA * 16 + FixedB, Rest};
getEscapedCharacter(<<$0, A, B, C, Rest/binary>>, LineNumber) when C =:= $x; C =:= $X ->
    FixedA = getOctalDigit(A, LineNumber),
    FixedB = getOctalDigit(B, LineNumber),
    FixedC = getOctalDigit(C, LineNumber),
    {FixedA * 8 * 8 + FixedB * 8 + FixedC, Rest};
getEscapedCharacter(<<$n, Rest/binary>>, _) ->
    {$\n, Rest};
getEscapedCharacter(<<$r, Rest/binary>>, _) ->
    {$\r, Rest};
getEscapedCharacter(<<$t, Rest/binary>>, _) ->
    {$\t, Rest};
getEscapedCharacter(<<$v, Rest/binary>>, _) ->
    {$\v, Rest};
getEscapedCharacter(<<$f, Rest/binary>>, _) ->
    {$\f, Rest};
getEscapedCharacter(<<$b, Rest/binary>>, _) ->
    {$\b, Rest};
getEscapedCharacter(<<C, _/binary>>, LineNumber) ->
    throw({LineNumber, {"invalid escaped character", C}}).

getHexDigit(C, _) when C >= $a, C =< $z ->
    C - $a + 10;
getHexDigit(C, _) when C >= $A, C =< $Z ->
    C - $A + 10;
getHexDigit(C, _) when C >= $0, C =< $9 ->
    C - $0;
getHexDigit(C, LineNumber) ->
    throw({LineNumber, {"invalid hex digit", C}}).

getOctalDigit(C, _) when C >= $0, C =< $7 ->
    C - $0;
getOctalDigit(C, LineNumber) ->
    throw({LineNumber, {"invalid hex digit", C}}).

spaceCharactersWithoutNewline() ->
    [$\s, $\r, $\t, $\v].

%% ++, --, ==, &&, ||, //, /*..*/, >>, >=, <=, <<
multiCharacterOperators() ->
    [$+, $-, $=, $&, $|, $/, $>, $<].

%% In current stage, newline is considered as token, too. The pre-processor need it.
singleCharacterOperators() ->
    [$\n, $^, $!, $*, $&, ${, $}, $[, $], $(, $)].

-spec tokenize(binary()) -> {ok, [token()]} | {error, lineNumber(), string()}.
tokenize(BinaryString) ->
    try
        {ok, getTokens(BinaryString, {1, []})}
    catch
        throw:{LineNumber, ErrorInfo} ->
            {error, LineNumber, ErrorInfo}
    end .

-ifdef(EUNIT).

operator_test() ->
    ?assertEqual({ok, [{'*', 1}, {'++', 1}, {'+', 1}, {newline, 1}, {'>>', 2}]}, tokenize(<<"*+++\n>>">>)).

string_test() ->
    ?assertEqual({ok, [{string, 1, "hello\nworld"}]}, tokenize(<<"\"hello\n\x77orld\"">>)).

-endif.

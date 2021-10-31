-module(cScanner).

-export([tokenize/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("./cScanner.hrl").

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
getMultiCharacterToken(<<$=, $=, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'==', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$=, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'=', CurrentLine} | Tokens]});
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
%% for preprocessor
getMultiCharacterToken(<<$#, $#, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'##', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$#, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'#', CurrentLine} | Tokens]}).

-spec getSingleCharacterToken(binary(), tokenContext()) -> [token()].
getSingleCharacterToken(<<$\n, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine + 1, [{newline, CurrentLine} | Tokens]});
%% escape + newline will ignore the newline
getSingleCharacterToken(<<$\\, $\n, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine + 1, Tokens});
getSingleCharacterToken(<<$\\, _/binary>>, {CurrentLine, _}) ->
    throw({CurrentLine, "invalid usage on \"\\\""});
getSingleCharacterToken(<<Character, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{list_to_atom([Character]), CurrentLine} | Tokens]}).

-spec getMiscellaneousToken(binary(), tokenContext()) -> [token()].
getMiscellaneousToken(<<$', Rest/binary>>, {CurrentLine, Tokens}) ->
    {CCharacter, RestContent} = getCharacterContent(Rest, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CCharacter | Tokens]});
getMiscellaneousToken(<<$", Rest/binary>>, {CurrentLine, Tokens}) ->
    {CString, RestContent, NewLineNumber} = getStringContent(Rest, CurrentLine),
    getTokens(RestContent, {NewLineNumber, [CString | Tokens]});
getMiscellaneousToken(<<C, _/binary>> = Content, {CurrentLine, Tokens}) when C >= $0, C =< $9 ->
    {CNumber, RestContent} = getNumber(Content, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CNumber | Tokens]});
getMiscellaneousToken(Content, {CurrentLine, Tokens}) ->
    {CIdentifier, RestContent} = getIdentifier(Content, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CIdentifier | Tokens]}).

-spec getNumber(binary(), lineNumber()) -> {cInteger(), binary()} | {cFloat(), binary()}.
getNumber(<<$0, $x, C, Rest/binary>>, CurrentLine) when C >= $0, C =< $9; C >= $a, C =< $f, C >= $A, C =< $F ->
    getHexNumber(<<C, Rest/binary>>, [], CurrentLine);
getNumber(<<$0, C, Rest/binary>>, CurrentLine) when C >= $0, C =< $7 ->
    getOctalNumber(<<C, Rest/binary>>, [], CurrentLine);
getNumber(<<$0, Rest/binary>>, CurrentLine) ->
    {{integer, CurrentLine, 0}, Rest};
getNumber(<<C, _/binary>> = Content, CurrentLine) when C >= $0, C =< $9  ->
    getDecimalNumber(Content, [], CurrentLine, true).

getHexNumber(<<C, Rest/binary>>, Cs, CurrentLine) when C >= $0, C =< $9; C >= $a, C =< $f, C >= $A, C =< $F ->
    getHexNumber(Rest, [C | Cs], CurrentLine);
getHexNumber(RestContent, Cs, CurrentLine) ->
    {{integer, CurrentLine, list_to_integer(lists:reverse(Cs), 16)}, RestContent}.

getOctalNumber(<<C, Rest/binary>>, Cs, CurrentLine) when C >= $0, C =< $7 ->
    getOctalNumber(Rest, [C | Cs], CurrentLine);
getOctalNumber(RestContent, Cs, CurrentLine) ->
    {{integer, CurrentLine, list_to_integer(lists:reverse(Cs), 8)}, RestContent}.

-spec getDecimalNumber(binary(), [char()], lineNumber(), boolean()) -> {cInteger(), binary()} | {cFloat(), binary()}.
getDecimalNumber(<<C, Rest/binary>>, Cs, CurrentLine, IsInteger) when C >= $0, C =< $9 ->
    getDecimalNumber(Rest, [C | Cs], CurrentLine, IsInteger);
getDecimalNumber(<<$., Rest/binary>>, Cs, CurrentLine, _) ->
    getDecimalNumber(Rest, [$. | Cs], CurrentLine, false);
%% in C language, 2e2 is valid, but for now, only 2.0e2 is allow. (this will be fixed in the future)
getDecimalNumber(<<C, Rest/binary>>, Cs, CurrentLine, _) when C =:= $E; C =:= $e ->
    getDecimalNumber(Rest, [C | Cs], CurrentLine, false);
getDecimalNumber(RestContent, Cs, CurrentLine, true) ->
    {{integer, CurrentLine, list_to_integer(lists:reverse(Cs))}, RestContent};
getDecimalNumber(RestContent, Cs, CurrentLine, false) ->
    {{float, CurrentLine, list_to_float(lists:reverse(Cs))}, RestContent}.

-spec getIdentifier(binary(), lineNumber()) -> {cIdentifier(), binary()}.
getIdentifier(<<C, Rest/binary>>, CurrentLine)  when C >= $a, C =< $z; C >= $A, C =< $Z; C =:= $_ ->
    {IdentifierCharacters, RestContent} = getIdentifierCharacters(Rest, []),
    {{identifier, CurrentLine, list_to_atom([C | IdentifierCharacters])}, RestContent};
getIdentifier(<<Character, _/binary>>, CurrentLine) ->
    throw({CurrentLine, cToolUtil:flatFmt("invalid identifier character: ~s", [[Character]])}).

-spec getIdentifierCharacters(binary(), [integer()]) -> {string(), binary()}.
getIdentifierCharacters(<<C, Rest/binary>>, CharacterCollect) when C >= $a, C =< $z; C >= $A, C =< $Z; C =:= $_; C >= $0, C =< $9 ->
    getIdentifierCharacters(Rest, [C | CharacterCollect]);
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
getCharacterContent(<<C, $', Rest/binary>>, CurrentLine) ->
    {{character, CurrentLine, C}, Rest};
getCharacterContent(<<C, _/binary>>, CurrentLine) when C =:= $\n; C =:= $\t ->
    throw({CurrentLine, "invalid character literal"});
getCharacterContent(<<$', _/binary>>, CurrentLine) ->
    throw({CurrentLine, "character is not found between quotation marks"});
getCharacterContent(_, CurrentLine) ->
    throw({CurrentLine, "missing \"'\" at the end of character"}).

%% For now, hex and octal only support fixed format: \xab\xab, \0abc\0abc. I am not sure whether this follows
%% the C standard, so this function will be changed in the future when necessary.
-spec getEscapedCharacter(binary(), lineNumber()) -> {integer(), binary()}.
getEscapedCharacter(<<C, A, B, Rest/binary>>, CurrentLine) when C =:= $x; C =:= $X ->
    FixedA = getHexDigit(A, CurrentLine),
    FixedB = getHexDigit(B, CurrentLine),
    {FixedA * 16 + FixedB, Rest};
getEscapedCharacter(<<$0, A, B, C, Rest/binary>>, CurrentLine) when C =:= $x; C =:= $X ->
    FixedA = getOctalDigit(A, CurrentLine),
    FixedB = getOctalDigit(B, CurrentLine),
    FixedC = getOctalDigit(C, CurrentLine),
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
getEscapedCharacter(<<C, _/binary>>, CurrentLine) ->
    throw({CurrentLine, {"invalid escaped character", C}}).

getHexDigit(C, _) when C >= $a, C =< $z ->
    C - $a + 10;
getHexDigit(C, _) when C >= $A, C =< $Z ->
    C - $A + 10;
getHexDigit(C, _) when C >= $0, C =< $9 ->
    C - $0;
getHexDigit(C, CurrentLine) ->
    throw({CurrentLine, {"invalid hex digit", C}}).

getOctalDigit(C, _) when C >= $0, C =< $7 ->
    C - $0;
getOctalDigit(C, CurrentLine) ->
    throw({CurrentLine, {"invalid hex digit", C}}).

spaceCharactersWithoutNewline() ->
    [$\s, $\r, $\t, $\v].

%% ++, --, ==, &&, ||, //, /*..*/, >>, >=, <=, <<, #, ##
multiCharacterOperators() ->
    [$+, $-, $=, $&, $|, $/, $>, $<, $#].

%% In current stage, newline is considered as token, too. The pre-processor need it.
singleCharacterOperators() ->
    [$\n, $^, $!, $*, $&, ${, $}, $[, $], $(, $), $?, $:, $,, $;, $\\].

-spec tokenize(binary()) -> {ok, [token()]} | {error, lineNumber(), string()}.
tokenize(BinaryString) ->
    try
        {ok, getTokens(BinaryString, {1, []})}
    catch
        throw:{LineNumber, ErrorInfo} ->
            {error, LineNumber, ErrorInfo}
    end.

-ifdef(EUNIT).

operator_test() ->
    ?assertEqual({ok, [{'*', 1}, {'++', 1}, {'+', 1}, {newline, 1}, {newline,2}, {'>>', 3}]}, tokenize(<<"*+++\n\n>>">>)).

integer_test() ->
    ?assertEqual({ok, [{integer, 1, 16}, {integer, 1, 8}, {integer, 1, 10}]}, tokenize(<<"0x10 010 10">>)).

float_test() ->
    ?assertEqual({ok, [{float, 1, 3.14}, {float, 1, 200.0}]}, tokenize(<<"3.14 2.0e2">>)).

string_test() ->
    ?assertEqual({ok, [{string, 1, "hello\nworld"}]}, tokenize(<<"\"hello\n\x77orld\"">>)).

character_test() ->
    ?assertEqual({ok, [{character, 1, 97}]}, tokenize(<<"'a'">>)),
    ?assertEqual({error, 1, "character is not found between quotation marks"}, tokenize(<<"''">>)).

identifier_test() ->
    ?assertEqual({ok, [{identifier, 1, hello}, {'=', 1}]}, tokenize(<<"hello =">>)).

preprocessor_test() ->
    ?assertEqual({ok, [{'##', 1}, {'#', 1}]}, tokenize(<<"###">>)).

lineContinue_test() ->
    ?assertEqual({ok, [{';', 1}, {';', 2}]}, tokenize(<<";\\\n;">>)),
    ?assertEqual({error, 1, "invalid usage on \"\\\""}, tokenize(<<";\\ \n;">>)).

-endif.

-module(cScanner).

-export([tokenize/1, tokensToBinaryString/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("./cScanner.hrl").

-type tokenContext() :: {lineNumber(), [token()]}.
-type textToScan() :: binary().

-spec getTokens(textToScan(), tokenContext()) -> [token()].
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

-spec getMultiCharacterToken(textToScan(), tokenContext()) -> [token()].
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
%% for comment
getMultiCharacterToken(<<$/, $/, Rest/binary>>, {CurrentLine, Tokens}) ->
    {CComment, RestContent} = getLineComment(Rest, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CComment | Tokens]});
getMultiCharacterToken(<<$/, $*, Rest/binary>>, {CurrentLine, Tokens}) ->
    {CComment, RestContent, NewLineNumber} = getBlockComment(Rest, CurrentLine),
    getTokens(RestContent, {NewLineNumber, [CComment | Tokens]});
getMultiCharacterToken(<<$/, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'/', CurrentLine} | Tokens]});
%% for preprocessor
getMultiCharacterToken(<<$#, $#, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'##', CurrentLine} | Tokens]});
getMultiCharacterToken(<<$#, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{'#', CurrentLine} | Tokens]}).

-spec getSingleCharacterToken(textToScan(), tokenContext()) -> [token()].
getSingleCharacterToken(<<$\n, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine + 1, [{newline, CurrentLine} | Tokens]});
%% escape + newline will ignore the newline
getSingleCharacterToken(<<$\\, $\n, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine + 1, Tokens});
getSingleCharacterToken(<<$\\, _/binary>>, {CurrentLine, _}) ->
    throw({CurrentLine, "invalid usage on \"\\\""});
getSingleCharacterToken(<<Character, Rest/binary>>, {CurrentLine, Tokens}) ->
    getTokens(Rest, {CurrentLine, [{list_to_atom([Character]), CurrentLine} | Tokens]}).

-spec getMiscellaneousToken(textToScan(), tokenContext()) -> [token()].
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
    {CIdentifier, RestContent} = getIdentifierOrKeyword(Content, CurrentLine),
    getTokens(RestContent, {CurrentLine, [CIdentifier | Tokens]}).

-spec getLineComment(textToScan(), lineNumber()) -> {cLineComment(), textToScan()}.
getLineComment(Content, CurrentLine) ->
    {CommentString, RestContent} = getLineComment(Content, CurrentLine, []),
    {{cLineComment, CurrentLine, CommentString}, RestContent}.

-spec getLineComment(textToScan(), lineNumber(), [byte()]) -> {CommentString :: binary(), textToScan()}.
getLineComment(<<$\n, _/binary>> = Rest, _, CollectedCharacters) ->
    {list_to_binary(lists:reverse(CollectedCharacters)), Rest};
getLineComment(<<C, Rest/binary>>, LineNumber, CollectedCharacters) ->
    getLineComment(Rest, LineNumber, [C | CollectedCharacters]).

-spec getBlockComment(textToScan(), lineNumber()) -> {cBlockComment(), textToScan(), lineNumber()}.
getBlockComment(Content, CurrentLine) ->
    {CommentString, RestContent, NewLineNumber} = getBlockComment(Content, CurrentLine, [], 0),
    {{cBlockComment, CurrentLine, CommentString}, RestContent, NewLineNumber}.

-spec getBlockComment(textToScan(), lineNumber(), [byte()], non_neg_integer()) -> {CommentString :: binary(), textToScan(), lineNumber()}.
getBlockComment(<<$*, $/, Rest/binary>>, LineNumber, CollectedCharacters, Depth) when Depth > 0 ->
    getBlockComment(Rest, LineNumber, [$/, $* | CollectedCharacters], Depth - 1);
getBlockComment(<<$*, $/, Rest/binary>>, LineNumber, CollectedCharacters, 0) ->
    {list_to_binary(lists:reverse(CollectedCharacters)), Rest, LineNumber};
getBlockComment(<<$/, $*, Rest/binary>>, LineNumber, CollectedCharacters, Depth) ->
    getBlockComment(Rest, LineNumber, [$*, $/ | CollectedCharacters], Depth + 1);
getBlockComment(<<$\n, Rest/binary>>, LineNumber, CollectedCharacters, Depth) ->
    getBlockComment(Rest, LineNumber + 1, [$\n | CollectedCharacters], Depth);
getBlockComment(<<C, Rest/binary>>, LineNumber, CollectedCharacters, Depth) ->
    getBlockComment(Rest, LineNumber, [C | CollectedCharacters], Depth).

-spec getNumber(textToScan(), lineNumber()) -> {cInteger(), textToScan()} | {cFloat(), textToScan()}.
getNumber(<<$0, $x, C, Rest/binary>>, CurrentLine) when C >= $0, C =< $9; C >= $a, C =< $f, C >= $A, C =< $F ->
    getHexNumber(<<C, Rest/binary>>, [], CurrentLine);
getNumber(<<$0, C, Rest/binary>>, CurrentLine) when C >= $0, C =< $7 ->
    getOctalNumber(<<C, Rest/binary>>, [], CurrentLine);
getNumber(<<$0, Rest/binary>>, CurrentLine) ->
    {{cInteger, CurrentLine, 0}, Rest};
getNumber(<<C, _/binary>> = Content, CurrentLine) when C >= $0, C =< $9  ->
    getDecimalNumber(Content, [], CurrentLine, true).

getHexNumber(<<C, Rest/binary>>, Cs, CurrentLine) when C >= $0, C =< $9; C >= $a, C =< $f, C >= $A, C =< $F ->
    getHexNumber(Rest, [C | Cs], CurrentLine);
getHexNumber(RestContent, Cs, CurrentLine) ->
    {{cInteger, CurrentLine, list_to_integer(lists:reverse(Cs), 16)}, RestContent}.

getOctalNumber(<<C, Rest/binary>>, Cs, CurrentLine) when C >= $0, C =< $7 ->
    getOctalNumber(Rest, [C | Cs], CurrentLine);
getOctalNumber(RestContent, Cs, CurrentLine) ->
    {{cInteger, CurrentLine, list_to_integer(lists:reverse(Cs), 8)}, RestContent}.

-spec getDecimalNumber(textToScan(), [byte()], lineNumber(), boolean()) -> {cInteger(), textToScan()} | {cFloat(), textToScan()}.
getDecimalNumber(<<C, Rest/binary>>, Cs, CurrentLine, IsInteger) when C >= $0, C =< $9 ->
    getDecimalNumber(Rest, [C | Cs], CurrentLine, IsInteger);
getDecimalNumber(<<$., Rest/binary>>, Cs, CurrentLine, _) ->
    getDecimalNumber(Rest, [$. | Cs], CurrentLine, false);
%% in C language, 2e2 is valid, but for now, only 2.0e2 is allow. (this will be fixed in the future)
getDecimalNumber(<<C, Rest/binary>>, Cs, CurrentLine, _) when C =:= $E; C =:= $e ->
    getDecimalNumber(Rest, [C | Cs], CurrentLine, false);
getDecimalNumber(RestContent, Cs, CurrentLine, true) ->
    {{cInteger, CurrentLine, list_to_integer(lists:reverse(Cs))}, RestContent};
getDecimalNumber(RestContent, Cs, CurrentLine, false) ->
    {{cFloat, CurrentLine, list_to_float(lists:reverse(Cs))}, RestContent}.

-spec getIdentifierOrKeyword(textToScan(), lineNumber()) -> {cIdentifier(), textToScan()}.
getIdentifierOrKeyword(<<C, Rest/binary>>, CurrentLine)  when C >= $a, C =< $z; C >= $A, C =< $Z; C =:= $_ ->
    {IdentifierCharacters, RestContent} = getIdentifierCharacters(Rest, []),
    IdentifierOrKeyword = list_to_atom([C | IdentifierCharacters]),
    case isElementOf(IdentifierOrKeyword, cKeywords()) of
        true ->
            {{IdentifierOrKeyword, CurrentLine}, RestContent};
        false ->
            {{cIdentifier, CurrentLine, IdentifierOrKeyword}, RestContent}
    end;
getIdentifierOrKeyword(<<Character, _/binary>>, CurrentLine) ->
    throw({CurrentLine, cToolUtil:flatFmt("invalid identifier character: ~s", [[Character]])}).

-spec getIdentifierCharacters(textToScan(), [integer()]) -> {string(), textToScan()}.
getIdentifierCharacters(<<C, Rest/binary>>, CharacterCollect) when C >= $a, C =< $z; C >= $A, C =< $Z; C =:= $_; C >= $0, C =< $9 ->
    getIdentifierCharacters(Rest, [C | CharacterCollect]);
getIdentifierCharacters(RestContent, CharacterCollect) ->
    {lists:reverse(CharacterCollect), RestContent}.

-spec getStringContent(textToScan(), lineNumber()) -> {cString(), textToScan(), lineNumber()}.
getStringContent(Content, CurrentLine) ->
    {String, RestContent, NewLineNumber} = getStringContent(Content, [], CurrentLine),
    {{cString, CurrentLine, String}, RestContent, NewLineNumber}.

-spec getStringContent(textToScan(), [integer()], lineNumber()) -> {binary(), textToScan(), lineNumber()}.
getStringContent(<<$", Rest/binary>>, CharacterCollect, CurrentLine) ->
    {list_to_binary(lists:reverse(CharacterCollect)), Rest, CurrentLine};
getStringContent(<<$\\, $\n, Rest/binary>>, CharacterCollect, CurrentLine) ->
    getStringContent(Rest, CharacterCollect, CurrentLine + 1);
getStringContent(<<$\n, _/binary>>, _, CurrentLine) ->
    throw({CurrentLine, "syntax error in string, newline is not allowed here"});
getStringContent(<<$\\, Rest/binary>>, CharacterCollect, CurrentLine) ->
    {Value, RestContent} = getEscapedCharacter(Rest, CurrentLine),
    getStringContent(RestContent, [Value | CharacterCollect], CurrentLine);
getStringContent(<<Character, Rest/binary>>, CharacterCollect, CurrentLine) ->
    getStringContent(Rest, [Character | CharacterCollect], CurrentLine).

-spec getCharacterContent(textToScan(), lineNumber()) -> {cCharacter(), textToScan()}.
getCharacterContent(<<$\\, Rest/binary>>, CurrentLine) ->
    {Value, RestContent} = getEscapedCharacter(Rest, CurrentLine),
    case RestContent of
        <<$', RealRestContent>> ->
            {{cCharacter, CurrentLine, Value}, RealRestContent};
        _ ->
            throw({CurrentLine, "missing \"'\" at the end of character"})
    end;
getCharacterContent(<<C, $', Rest/binary>>, CurrentLine) ->
    {{cCharacter, CurrentLine, C}, Rest};
getCharacterContent(<<C, _/binary>>, CurrentLine) when C =:= $\n; C =:= $\t ->
    throw({CurrentLine, "invalid character literal"});
getCharacterContent(<<$', _/binary>>, CurrentLine) ->
    throw({CurrentLine, "character is not found between quotation marks"});
getCharacterContent(_, CurrentLine) ->
    throw({CurrentLine, "missing \"'\" at the end of character"}).

%% For now, hex and octal only support fixed format: \xab\xab, \0abc\0abc. I am not sure whether this follows
%% the C standard, so this function will be changed in the future when necessary.
-spec getEscapedCharacter(textToScan(), lineNumber()) -> {integer(), textToScan()}.
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
getEscapedCharacter(<<$a, Rest/binary>>, _) ->
    {7, Rest};
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

%% there are 32 keywords in C language
cKeywords() ->
    ['if', else, switch, 'case', default, for, while, do, break, continue, return, goto, struct, enum, union, sizeof, typedef,
     const, static, extern, volatile, auto, register, signed, unsigned, char, short, int, long, double, float, void].

-spec tokenize(textToScan()) -> {ok, [token()]} | {error, lineNumber(), string()}.
tokenize(BinaryString) ->
    try
        {ok, getTokens(BinaryString, {1, []})}
    catch
        throw:{LineNumber, ErrorInfo} ->
            {error, LineNumber, ErrorInfo}
    end.

-ifdef(EUNIT).

operator_test() ->
    ?assertEqual({ok, [{'*', 1}, {'++', 1}, {'+', 1}, {newline, 1}, {newline,2}, {'>>', 3}]},
                 tokenize(<<"*+++\n\n>>">>)).

integer_test() ->
    ?assertEqual({ok, [{cInteger, 1, 16}, {cInteger, 1, 8}, {cInteger, 1, 10}]},
                 tokenize(<<"0x10 010 10">>)).

float_test() ->
    ?assertEqual({ok, [{cFloat, 1, 3.14}, {cFloat, 1, 200.0}]},
                 tokenize(<<"3.14 2.0e2">>)).

string_test() ->
    ?assertEqual({ok, [{cString, 1, <<"helloworld">>}]},
                 tokenize(<<"\"hello\\\n\x77orld\"">>)).

string_error_test() ->
    ?assertEqual({error, 1, "syntax error in string, newline is not allowed here"},
                 tokenize(<<"\"hello\n\x77orld\"">>)).

character_test() ->
    ?assertEqual({ok, [{cCharacter, 1, 97}]},
                 tokenize(<<"'a'">>)),
    ?assertEqual({error, 1, "character is not found between quotation marks"},
                 tokenize(<<"''">>)).

identifier_test() ->
    ?assertEqual({ok, [{cIdentifier, 1, hello}, {'=', 1}]},
                 tokenize(<<"hello =">>)).

preprocessor_test() ->
    ?assertEqual({ok, [{'##', 1}, {'#', 1}]},
                 tokenize(<<"###">>)).

lineContinue_test() ->
    ?assertEqual({ok, [{';', 1}, {';', 2}]},
                 tokenize(<<";\\\n;">>)),
    ?assertEqual({error, 1, "invalid usage on \"\\\""},
                 tokenize(<<";\\ \n;">>)).

lineComment_test() ->
    ?assertEqual({ok, [{cLineComment, 1, <<"hello">>}, {newline, 1}]},
                 tokenize(<<"//hello\n">>)).

blockComment_test() ->
    ?assertEqual({ok, [{cBlockComment, 1, <<"hello\nworld\n">>}, {newline, 3}]},
                 tokenize(<<"/*hello\nworld\n*/\n">>)).

%% nested block comment is not supported by Standard C, but supported by this compiler.
blockComment_nested_test() ->
    ?assertEqual({ok, [{cBlockComment, 1, <<"/*hello*/">>}, {newline, 1}]},
        tokenize(<<"/*/*hello*/*/\n">>)).

-endif.

-spec tokensToBinaryString([token()]) -> binary().
tokensToBinaryString(Tokens) ->
    list_to_binary(lists:join(" ", lists:map(fun tokenToString/1, Tokens))).

-spec tokenToString(token()) -> string().
tokenToString({cInteger, _, Number}) ->
    integer_to_list(Number);
tokenToString({cFloat, _, Number}) ->
    float_to_list(Number);
tokenToString({cCharacter, _, Number}) ->
    [$', Number, $'];
tokenToString({cString, _, String}) ->
    String;
tokenToString({cIdentifier, _, Name}) ->
    atom_to_list(Name);
tokenToString({newline, _}) ->
    $\n;
tokenToString({AnyAtom, _}) ->
    atom_to_list(AnyAtom).

-ifdef(EUNIT).

tokensToBinaryString_test() ->
    ?assertEqual(<<"* ++ + \n \n >>">>,
                 tokensToBinaryString([{'*', 1}, {'++', 1}, {'+', 1}, {newline, 1}, {newline,2}, {'>>', 3}])).

-endif.
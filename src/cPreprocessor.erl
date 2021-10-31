-module(cPreprocessor).

-export([process/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("./cScanner.hrl").

-type macroMap() :: #{atom() => [token()]}.
-type preprocessContext() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], EndTag :: else | endif | normal}.
-type handleReturn() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], RestTokens :: [token()]}.

-spec handleSpecial([token()], preprocessContext()) -> handleReturn().
handleSpecial([{identifier, _, define}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    case MacroMap of
        #{Name := _} ->
            throw({LineNumber, cToolUtil:flatFmt("macro name conflict: \"~s\"", [Name])});
        _ ->
            {Tokens, RestTokens} = getExpressionTillEOL(Rest),
            handleNormal(RestTokens, {MacroMap#{Name => Tokens}, TokensToReturn, EndTag})
    end;
handleSpecial([{identifier, _, undef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    handleNormal(Rest, {maps:remove(Name, MacroMap), TokensToReturn, EndTag});
handleSpecial([{identifier, _, ifdef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context);
                                                        _ ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{identifier, LineNumber, ifdef} | _], _) ->
    throw({LineNumber, "invalid #ifdef command"});
handleSpecial([{identifier, _, ifndef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context);
                                                        _ ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{identifier, LineNumber, ifndef} | _], _) ->
    throw({LineNumber, "invalid #ifndef command"});
handleSpecial([{identifier, _, 'if'} | Rest], {MacroMap, _, EndTag} = Context) ->
    {Tokens, RestTokens} = getExpressionTillEOL(Rest),
    {MacroMapNew, CollectedTokens, RestTokensNew} = case evaluateTokenExpressions(Tokens, MacroMap) of
                                                        true ->
                                                            collectToElseAndIgnoreToEndif(RestTokens, Context);
                                                        false ->
                                                            ignoreToElseAndCollectToEndif(RestTokens, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{identifier, _, else}, {newline, _} | RestContent], {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, RestContent};
handleSpecial([{identifier, LineNumber, else}, {newline, _} | _], {_, _, normal}) ->
    throw({LineNumber, "\"#else\" is not expected here"});
handleSpecial([{identifier, LineNumber, else} | _], _) ->
    throw({LineNumber, "syntax error on \"#else\""});
handleSpecial([{identifier, _, endif}, {newline, _} | RestContent], {MacroMap, TokensToReturn, endif}) ->
    {MacroMap, TokensToReturn, RestContent};
%% when the "#else" part is missing ("#if" following "#endif"), pretend that the "#else\n" exists and has been swallowed,
%% and put the "#endif" back to unhandled tokens.
handleSpecial([{identifier, LineNumber, endif}, {newline, _} | _] = Content, {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, [{'#', LineNumber} | Content]};
handleSpecial([{identifier, LineNumber, endif}] = Content, {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, [{'#', LineNumber} | Content]};
handleSpecial([{identifier, LineNumber, endif}, {newline, _} | _], {_, _, normal}) ->
    throw({LineNumber, "\"#endif\" is not expected here"});
handleSpecial([{identifier, LineNumber, endif} | _], _) ->
    throw({LineNumber, "syntax error on \"#endif\""});
handleSpecial([{identifier, LineNumber, error} | _], _) ->
    throw({LineNumber, "compile error... (todo)"});
handleSpecial([{identifier, _, include} | Rest], Context) ->
    handleNormal(Rest, Context);
handleSpecial([{identifier, LineNumber, Name} | _], _) ->
    throw({LineNumber, cToolUtil:flatFmt("unexpected operator \"~s\" here", [Name])});
handleSpecial([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handleSpecial([], {_, _, EndTag}) ->
    throw({0, cToolUtil:flatFmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec collectToElseAndIgnoreToEndif([token()], preprocessContext()) -> handleReturn().
collectToElseAndIgnoreToEndif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% collect "then" part
    {MacroMapNew, CollectedTokens, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% ignore "else" part
    {_, _, RestTokens} = handleNormal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec ignoreToElseAndCollectToEndif([token()], preprocessContext()) -> handleReturn().
ignoreToElseAndCollectToEndif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% ignore "then" part
    {_, _, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% collect "else" part
    {MacroMapNew, CollectedTokens, RestTokens} = handleNormal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec handleNormal([token()], preprocessContext()) -> handleReturn().
handleNormal([{'#', _} | Rest], Context) ->
    handleSpecial(Rest, Context);
handleNormal([{newline, _} | Rest], Context) ->
    handleNormal(Rest, Context);
handleNormal([Token | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    handleNormal(Rest, {MacroMap, [Token | TokensToReturn], EndTag});
handleNormal([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handleNormal([], {_, _, EndTag}) ->
    throw({0, cToolUtil:flatFmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec process([token()]) -> [token()].
process(Tokens) ->
    {_, ProcessedTokens, _} = handleNormal(convertElifToElseAndIf(Tokens), {#{}, [], normal}),
    lists:reverse((ProcessedTokens)).

-spec getExpressionTillEOL([token()]) -> {[token()], [token()]}.
getExpressionTillEOL(Tokens) ->
    getExpressionTillEOL(Tokens, []).

-spec getExpressionTillEOL([token()], [token()]) -> {[token()], [token()]}.
getExpressionTillEOL([{newline, _} | Rest], CollectedTokens) ->
    {lists:reverse(CollectedTokens), Rest};
getExpressionTillEOL([Token | Rest], CollectedTokens) ->
    getExpressionTillEOL(Rest, [Token | CollectedTokens]).

%% tokens should be parsed to ast before evaluating them, this function will be updated when the parser is finished
evaluateTokenExpressions([{integer, _, 0}], _MacroMap) ->
    false;
evaluateTokenExpressions([{integer, _, 1}], _MacroMap) ->
    true.

-ifdef(EUNIT).

process_noOperator_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"int a = 1;">>),
    ?assertEqual([{identifier, 1, int}, {identifier, 1, a}, {'=', 1}, {integer, 1, 1}, {';', 1}], process(Tokens)).

process_if_true_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_if_false_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_false_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#ifdef BLAH\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_true_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#define BLAH\n #ifdef BLAH\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_ifndef_false_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#ifndef BLAH\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_ifndef_true_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#define BLAH\n #ifndef BLAH\n a\n #else\n b\n #endif">>),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_1_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif">>),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_recursive_2_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n #if 0\n a\n #else\n b\n #endif\n #else\n c\n #endif">>),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_3_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif">>),
    ?assertEqual([{identifier, 8, c}], process(Tokens)).

process_elif_1_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n a\n #elif 1\n b\n #endif">>),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_elif_2_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #elif 0\n b\n #elif 0\n c\n #else\n d\n #endif">>),
    ?assertEqual([{identifier, 8, d}], process(Tokens)).

process_elif_3_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif">>),
    ?assertEqual([{identifier, 6, c}], process(Tokens)).

process_elif_4_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #elif 1\n b\n #elif 1\n c\n #else\n d\n #endif">>),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

-endif.

-spec convertElifToElseAndIf([token()]) -> [token()].
convertElifToElseAndIf(Tokens) ->
    lists:reverse(lists:flatten(convertElifToElseAndIf(Tokens, [], 0))).

%% after this process, all "#endif" will have a newline after it.
-spec convertElifToElseAndIf([token()], TokenTree, integer()) -> TokenTree when TokenTree :: [token() | TokenTree].
convertElifToElseAndIf([{'#', _} = PreTag, {identifier, _, 'if'} = Token | Rest], CollectedTokens, _) ->
    [convertElifToElseAndIf(Rest, [], 0), Token, PreTag, CollectedTokens];
convertElifToElseAndIf([{'#', _}, {identifier, LineNumber, elif} | Rest], CollectedTokens, ElifDepth) ->
    convertElifToElseAndIf(Rest, [makeElifReplacement(LineNumber) | CollectedTokens], ElifDepth + 1);
convertElifToElseAndIf([{'#', _} = PreTag, {identifier, LineNumber, endif} = Token | Rest], CollectedTokens, ElifDepth) ->
    convertElifToElseAndIf(Rest, [lists:duplicate(ElifDepth + 1, [{newline, LineNumber}, Token, PreTag]) | CollectedTokens], 0);
convertElifToElseAndIf([Token | Rest], CollectedTokens, ElifDepth) ->
    convertElifToElseAndIf(Rest, [Token | CollectedTokens], ElifDepth);
convertElifToElseAndIf([], _, N) when N =/= 0 ->
    throw({0, "preprocessor error, #if and #endif mismatch"});
convertElifToElseAndIf([], CollectedTokens, 0) ->
    CollectedTokens.

-spec makeElifReplacement(integer()) -> [token()].
makeElifReplacement(LineNumber) ->
    lists:reverse([{'#', LineNumber}, {identifier, LineNumber, else}, {newline, LineNumber}, {'#', LineNumber}, {identifier, LineNumber, 'if'}]).

-ifdef(EUNIT).

convertElifToElseAndIf_1_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n a\n #elif 1\n b\n #endif">>),
    ?assertEqual(<<"# if 1 \n a \n # else \n # if 1 \n b \n # endif \n # endif \n">>,
                 cScanner:tokensToBinaryString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_2_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif">>),
    ?assertEqual(<<"# if 0 \n a \n # else \n # if 0 \n b \n # else \n # if 1 \n c \n # else \n d \n # endif \n # endif \n # endif \n">>,
                 cScanner:tokensToBinaryString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_3_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n #else\n #endif">>),
    ?assertEqual(<<"# if 0 \n # else \n # endif \n">>,
                 cScanner:tokensToBinaryString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_4_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n #if 1\n #else\n #endif #endif">>),
    ?assertEqual(<<"# if 0 \n # if 1 \n # else \n # endif \n # endif \n">>,
                 cScanner:tokensToBinaryString(convertElifToElseAndIf(Tokens))).

-endif.
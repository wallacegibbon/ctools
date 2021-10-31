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
handleSpecial([{identifier, _, ifdef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, TokensToReturn, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context);
                                                        _ ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens ++ TokensToReturn, EndTag});
handleSpecial([{identifier, LineNumber, ifdef} | _], _) ->
    throw({LineNumber, "invalid #ifdef command"});
handleSpecial([{identifier, _, ifndef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, TokensToReturn, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context);
                                                        _ ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens ++ TokensToReturn, EndTag});
handleSpecial([{identifier, LineNumber, ifndef} | _], _) ->
    throw({LineNumber, "invalid #ifndef command"});
handleSpecial([{identifier, _, 'if'} | Rest], {MacroMap, TokensToReturn, EndTag} = Context) ->
    {Tokens, RestTokens} = getExpressionTillEOL(Rest),
    {MacroMapNew, CollectedTokens, RestTokensNew} = case evaluateTokenExpressions(Tokens, MacroMap) of
                                                        true ->
                                                            collectToElseAndIgnoreToEndif(RestTokens, Context);
                                                        false ->
                                                            ignoreToElseAndCollectToEndif(RestTokens, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens ++ TokensToReturn, EndTag});
handleSpecial([{identifier, _, else}, {newline, _} | RestContent], {MacroMap, TokensToReturn, EndTag})
        when EndTag =:= else; EndTag =:= else ->
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
collectToElseAndIgnoreToEndif(Tokens, {MacroMap, _, _}) ->
    %% collect "then" part
    {MacroMapNew, CollectedTokens, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% ignore "else" part
    {_, _, RestTokens} = handleNormal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens, RestTokens}.

-spec ignoreToElseAndCollectToEndif([token()], preprocessContext()) -> handleReturn().
ignoreToElseAndCollectToEndif(Tokens, {MacroMap, _, _}) ->
    %% ignore "then" part
    {_, _, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% collect "else" part
    handleNormal(RestTokensRaw, {MacroMap, [], endif}).

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
%%    {_, ProcessedTokens, _} = handleNormal(Tokens, {#{}, [], normal}),
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
    lists:reverse(lists:flatten(convertElifToElseAndIf(Tokens, {0, []}))).


%% after this process, all "#endif" will have a newline after it.
-spec convertElifToElseAndIf([token()], {integer(), [token()]}) -> [token() | any()].
convertElifToElseAndIf([{'#', _} = PreTag, {identifier, _, 'if'} = Token | Rest], {_, CollectedTokens}) ->
    [convertElifToElseAndIf(Rest, {0, []}), Token, PreTag, CollectedTokens];
convertElifToElseAndIf([{'#', _}, {identifier, LineNumber, elif} | Rest], {ElifDepth, CollectedTokens}) ->
    convertElifToElseAndIf(Rest, {ElifDepth + 1, makeElifReplacement(LineNumber) ++ CollectedTokens});
convertElifToElseAndIf([{'#', _} = PreTag, {identifier, LineNumber, endif} = Token | Rest], {ElifDepth, CollectedTokens}) ->
    convertElifToElseAndIf(Rest, {0, lists:duplicate(ElifDepth + 1, [{newline, LineNumber}, Token, PreTag]) ++ CollectedTokens});
convertElifToElseAndIf([Token | Rest], {ElifDepth, CollectedTokens}) ->
    convertElifToElseAndIf(Rest, {ElifDepth, [Token | CollectedTokens]});
convertElifToElseAndIf([], {N, _}) when N =/= 0 ->
    throw({0, "preprocessor error, #if and #endif mismatch"});
convertElifToElseAndIf([], {0, CollectedTokens}) ->
    CollectedTokens.

-spec makeElifReplacement(integer()) -> [token()].
makeElifReplacement(LineNumber) ->
    lists:reverse([{'#', LineNumber}, {identifier, LineNumber, else}, {newline, LineNumber}, {'#', LineNumber}, {identifier, LineNumber, 'if'}]).

-ifdef(EUNIT).

convertElifToElseAndIf_1_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\n a\n #elif 1\n b\n #endif">>),
    ?assertEqual([{'#', 1}, {identifier, 1, 'if'}, {integer, 1, 1}, {newline, 1},
                  {identifier, 2, a}, {newline, 2},
                  %% elif
                  {'#', 3}, {identifier, 3, else}, {newline, 3},
                  {'#', 3}, {identifier, 3, 'if'}, {integer, 3, 1}, {newline, 3},
                  %%
                  {identifier, 4, b}, {newline, 4},
                  {'#', 5}, {identifier, 5, endif}, {newline, 5}, {'#', 5}, {identifier, 5, endif}, {newline, 5}],
                 convertElifToElseAndIf(Tokens)).

convertElifToElseAndIf_2_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif">>),
    ?assertEqual([{'#', 1}, {identifier, 1, 'if'}, {integer, 1, 0}, {newline, 1},
                  {identifier, 2, a}, {newline, 2},
                  %% elif
                  {'#', 3}, {identifier, 3, else}, {newline, 3},
                  {'#', 3}, {identifier, 3, 'if'}, {integer, 3, 0}, {newline, 3},
                  %%
                  {identifier, 4, b}, {newline, 4},
                  %% elif
                  {'#', 5}, {identifier, 5, else}, {newline, 5},
                  {'#', 5}, {identifier, 5, 'if'}, {integer, 5, 1}, {newline, 5},
                  %%
                  {identifier, 6, c}, {newline, 6},
                  {'#', 7}, {identifier, 7, else}, {newline, 7},
                  {identifier, 8, d}, {newline, 8},
                  {'#', 9}, {identifier, 9, endif}, {newline, 9},
                  {'#', 9}, {identifier, 9, endif}, {newline, 9},
                  {'#', 9}, {identifier, 9, endif}, {newline, 9}],
                 convertElifToElseAndIf(Tokens)).

convertElifToElseAndIf_3_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n #else\n #endif">>),
    ?assertEqual([{'#', 1}, {identifier, 1, 'if'}, {integer, 1, 0}, {newline, 1},
                  {'#', 2}, {identifier, 2, else}, {newline, 2},
                  {'#', 3}, {identifier, 3, endif}, {newline, 3}],
                 convertElifToElseAndIf(Tokens)).

convertElifToElseAndIf_4_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 0\n #if 1\n #else\n #endif #endif">>),
    ?assertEqual([{'#', 1}, {identifier, 1, 'if'}, {integer, 1, 0}, {newline, 1},
                  {'#', 2}, {identifier, 2, 'if'}, {integer, 2, 1}, {newline, 2},
                  {'#', 3}, {identifier, 3, else}, {newline, 3},
                  {'#', 4}, {identifier, 4, endif}, {newline, 4},
                  {'#', 4}, {identifier, 4, endif}, {newline, 4}],
                 convertElifToElseAndIf(Tokens)).

-endif.
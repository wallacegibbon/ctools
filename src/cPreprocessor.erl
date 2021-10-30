-module(cPreprocessor).

-export([handleNormal/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("./cScanner.hrl").

-type controlFlag() :: collectTokensUntilElse | collectTokensUntilEndif | ignoreTokensUntilElse | nothing.
-type macroMap() :: #{atom() => [token()]}.
-type preprocessContext() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], EndFlag :: controlFlag()}.
-type handleReturn() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], RestTokens :: [token()]}.

-spec handleSpecial([token()], preprocessContext()) -> handleReturn().
handleSpecial([{identifier, _, define}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndFlag}) ->
    case MacroMap of
        #{Name := _} ->
            throw({LineNumber, cToolUtil:flatFmt("macro name conflict: \"~s\"", [Name])});
        _ ->
            {Tokens, RestTokens} = getExpressionTillEOL(Rest),
            handleNormal(RestTokens, {MacroMap#{Name => Tokens}, TokensToReturn, EndFlag})
    end;
handleSpecial([{identifier, _, undef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, TokensToReturn, EndFlag}) ->
    handleNormal(Rest, {maps:remove(Name, MacroMap), TokensToReturn, EndFlag});
handleSpecial([{identifier, _, ifdef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap, _, _} = Context) ->
    case MacroMap of
        #{Name := _} ->
            collectTokensAndIgnoreElse(Rest, Context);
        _ ->
            ignoreTokensAndCollectElse(Rest, Context)
    end;
handleSpecial([{identifier, LineNumber, ifdef} | _], _) ->
    throw({LineNumber, "invalid #ifdef command"});
handleSpecial([{identifier, _, ifndef}, {identifier, _, Name}, {newline, _} | Rest], {MacroMap} = Context) ->
    case MacroMap of
        #{Name := _} ->
            ignoreTokensAndCollectElse(Rest, Context);
        _ ->
            collectTokensAndIgnoreElse(Rest, Context)
    end;
handleSpecial([{identifier, LineNumber, ifndef} | _], _) ->
    throw({LineNumber, "invalid #ifndef command"});
handleSpecial([{identifier, _, CommonIf} | Rest], {MacroMap, _, _} = Context) when CommonIf =:= 'if'; CommonIf =:= elif ->
    {Tokens, RestTokens} = getExpressionTillEOL(Rest),
    case evaluateTokenExpressions(Tokens, MacroMap) of
        true ->
            collectTokensAndIgnoreElse(RestTokens, Context);
        false ->
            ignoreTokensAndCollectElse(RestTokens, Context)
    end;
handleSpecial([{identifier, _, else}, {newline, _} | RestContent], {MacroMap, TokensToReturn, collectTokensUntilElse}) ->
    {MacroMap, TokensToReturn, RestContent};
handleSpecial([{identifier, LineNumber, else}, {newline, _} | _], {_, _, nothing}) ->
    throw({LineNumber, "\"#else\" is not expected here"});
handleSpecial([{identifier, LineNumber, else} | _], _) ->
    throw({LineNumber, "syntax error on \"#else\""});
handleSpecial([{identifier, _, endif}, {newline, _} | RestContent], {MacroMap, TokensToReturn, collectTokensUntilEndif}) ->
    {MacroMap, TokensToReturn, RestContent};
handleSpecial([{identifier, LineNumber, else}, {newline, _} | _], {_, _, nothing}) ->
    throw({LineNumber, "\"#endif\" is not expected here"});
handleSpecial([{identifier, LineNumber, endif} | _], _) ->
    throw({LineNumber, "syntax error on \"#else\""});
handleSpecial([{identifier, LineNumber, error} | _], _) ->
    throw({LineNumber, "compile error... (todo)"});
handleSpecial([{identifier, _, include} | Rest], Context) ->
    handleNormal(Rest, Context);
handleSpecial([{identifier, LineNumber, Name} | _], _) ->
    throw({LineNumber, cToolUtil:flatFmt("unexpected operator \"~s\" here", [Name])});
handleSpecial([], {MacroMap, TokensToReturn, _}) ->
    {MacroMap, TokensToReturn, []}.

-spec collectTokensAndIgnoreElse([token()], preprocessContext()) -> handleReturn().
collectTokensAndIgnoreElse(Tokens, {MacroMap, TokensToReturn, EndFlag}) ->
    {MacroMapNew, TokensOfThenPart, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], collectTokensUntilElse}),
    handleNormal(forwardUntilEndif(RestTokensRaw), {MacroMapNew, TokensOfThenPart ++ TokensToReturn, EndFlag}).

-spec ignoreTokensAndCollectElse([token()], preprocessContext()) -> handleReturn().
ignoreTokensAndCollectElse(Tokens, {MacroMap, _, _}) ->
    case forwardUntilElse(Tokens) of
        {unfinished, RestTokens} ->
            handleNormal(RestTokens, {MacroMap, [], collectTokensUntilEndif});
        {finished, RestTokens} ->
            handleNormal(RestTokens, {MacroMap, [], nothing})
    end.

-spec handleNormal([token()]) -> handleReturn().
handleNormal(Tokens) ->
    {_, ProcessedTokens, _} = handleNormal(Tokens, {#{}, [], nothing}),
    lists:reverse(ProcessedTokens).

-spec handleNormal([token()], preprocessContext()) -> handleReturn().
handleNormal([{'#', _} | Rest], Context) ->
    handleSpecial(Rest, Context);
handleNormal([Token | Rest], {MacroMap, TokensToReturn, EndFlag}) ->
    handleNormal(Rest, {MacroMap, [Token | TokensToReturn], EndFlag});
handleNormal([{newline, _} | Rest], Context) ->
    handleNormal(Rest, Context);
handleNormal([], {MacroMap, TokensToReturn, _}) ->
    {MacroMap, TokensToReturn, []}.

-spec getExpressionTillEOL([token()]) -> {[token()], [token()]}.
getExpressionTillEOL(Tokens) ->
    getExpressionTillEOL(Tokens, []).

-spec getExpressionTillEOL([token()], [token()]) -> {[token()], [token()]}.
getExpressionTillEOL([{newline, _} | Rest], CollectedTokens) ->
    {CollectedTokens, Rest};
getExpressionTillEOL([Token | Rest], CollectedTokens) ->
    getExpressionTillEOL(Rest, [Token | CollectedTokens]).

%% ignore the if part
-spec forwardUntilElse([token()]) -> {unfinished, [token()]} | {finished, [token()]}.
forwardUntilElse([{'#', _}, {identifier, _, endif}, {newline, _} | Rest]) ->
    {finished, Rest};
forwardUntilElse([{'#', _}, {identifier, LineNumber, endif} | _]) ->
    throw({LineNumber, "there should be a new line after #endif"});
forwardUntilElse([{'#', _}, {identifier, _, else}, {newline, _} | Rest]) ->
    {unfinished, Rest};
forwardUntilElse([{'#', _}, {identifier, LineNumber, else} | _]) ->
    throw({LineNumber, "there should be a new line after #else"});
%% The "#elif" contains condition expressions, treat it just as a "#if"
forwardUntilElse([{'#', _}, {identifier, _, elif} | _] = Rest) ->
    {finished, Rest};
forwardUntilElse([_ | Rest]) ->
    forwardUntilElse(Rest);
forwardUntilElse([]) ->
    throw({0, "missing #endif"}).

-spec forwardUntilEndif([token()]) -> token().
forwardUntilEndif([{'#', _}, {identifier, _, endif}, {newline, _} | Rest]) ->
    Rest;
forwardUntilEndif([{'#', _}, {identifier, LineNumber, endif} | _]) ->
    throw({LineNumber, "there should be a new line after #endif"});
forwardUntilEndif([_ | Rest]) ->
    forwardUntilEndif(Rest);
forwardUntilEndif([]) ->
    throw({0, "missing #endif"}).

%% tokens should be parsed to ast before evaluating them, this function will be updated when the parser is finished
evaluateTokenExpressions([{integer, _, 0}], _MacroMap) ->
    false;
evaluateTokenExpressions([{integer, _, 1}], _MacroMap) ->
    true.

-ifdef(EUNIT).

handleNormal_noOperator_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"int a = 1;">>),
    ?assertEqual([{identifier, 1, int}, {identifier, 1, a}, {'=', 1}, {integer, 1, 1}, {';', 1}], handleNormal(Tokens)).

handleNormal_if_true_test() ->
    {ok, Tokens} = cScanner:tokenize(<<"#if 1\na\n#else\nb\n#endif\n">>),
    ?assertEqual([{identifier, 1, a}], handleNormal(Tokens)).

-endif.

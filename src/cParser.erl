-module(cParser).

-include("./cScanner.hrl").
-include("./cAST.hrl").

-export([parse/1]).

-type ast() :: any().

-spec parse([token()]) -> {ok, [ast()]} | {error, lineNumber(), string()}.
parse(_Tokens) ->
    {ok, []}.


getExpression([{identifier, _, Name} | Rest]) ->
    getOperator(Rest);
getExpression([{'*', LineNumber} | Rest]) ->
    #commonExpression1{operator = '*a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'&', LineNumber} | Rest]) ->
    #commonExpression1{operator = '&a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'!', LineNumber} | Rest]) ->
    #commonExpression1{operator = '!a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'~', LineNumber} | Rest]) ->
    #commonExpression1{operator = '~a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'++', LineNumber} | Rest]) ->
    #commonExpression1{operator = '++a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'--', LineNumber} | Rest]) ->
    #commonExpression1{operator = '--a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'+', LineNumber} | Rest]) ->
    #commonExpression1{operator = '+a', operand = getExpression(Rest), line = LineNumber};
getExpression([{'-', LineNumber} | Rest]) ->
    #commonExpression1{operator = '-a', operand = getExpression(Rest), line = LineNumber};
getExpression([Any | _]) ->
    throw({element(2, Any), cToolUtil:flatFmt("unexpected token \"~s\" is found here", [element(1, Any)])}).

operatorPriorityMap() ->
    #{'a->b' => 1, 'a.b' => 1, 'a(b)' => 1, 'a[b]' => 1,
      '!a' => 2, '~a' => 2, '*a' => 2, '&a' => 2, 'a++' => 2, '++a' => 2, 'a--' => 2, '--a' => 2, '+a' => 2, '-a' => 2, '(a)b' => 2,
      'a*b' => 3, 'a/b' => 3, 'a%b' => 3,
      'a+b' => 4, 'a-b' => 4,
      'a<<b' => 5, 'a>>b' => 5,
      'a>b' => 6, 'a>=b' => 6, 'a<b' => 6, 'a<=b' => 6,
      'a==b' => 7, 'a!=b' => 7,
      'a&b' => 8, 'a^b' => 9, 'a|b' => 10, 'a&&b' => 11, 'a||b' => 12, 'a?b:c' => 13, 'a=b' => 14, 'a,b' => 15}.

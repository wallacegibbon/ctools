-type commonExpression2Type() :: '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '&&' | '||' | '==' | '!=' | '=' | '->' | '.' | '[]'.
-type commonExpression1Type() :: '+' | '-' | '++' | '--' | '!' | '*' | '&' | sizeof.
-type lineNumber() :: pos_integer().

-record(commonExpression2,
        {operator = '+' :: commonExpression2Type(),
         operand1 = [] :: cAST(),
         operand2 = [] :: cAST(),
         line = 0:: lineNumber()}).

-record(commonExpression1,
        {operator = '+' :: commonExpression1Type(),
         operand = [] :: cAST(),
         line = 0:: lineNumber()}).

-record(functionCall,
        {functionName = "" :: string(),
         body = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(ifStatement,
        {condition = [] :: cExpression(),
         then = [] :: cAST(),
         else = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(switchStatement,
        {condition = [] :: cExpression(),
         cases = [] :: [{CaseValue :: cExpression(), CaseStatements :: cAST()}],
         default = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(whileStatement,
        {condition = [] :: cExpression(),
         line = 0 :: lineNumber()}).

-record(doWhileStatement,
        {condition = [] :: cExpression(),
         line = 0 :: lineNumber()}).

-record(forStatement,
        {initialize = [] :: #expressionStatement{},
         condition = [] :: cExpression(),
         tail = [] :: cExpression(),
         body = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(expressionStatement,
        {expression :: cExpression(),
         line = 0 :: lineNumber()}).

-type cExpression() :: #commonExpression2{} | #commonExpression1{} | #functionCall{}.
-type cStatement() :: #ifStatement{} | #switchStatement{} | #whileStatement{} | #doWhileStatement{} | #expressionStatement{}.
-type cAST() :: [cStatement()].

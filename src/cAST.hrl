-ifndef(__cAST_hrl__).
-define(__cAST_hrl__, true).

-include("./cScanner.hrl").

-type commonExpression2Type() :: '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '&&' | '||' | '==' | '!=' | '=' | '->' | '.' | '[]' | '()'.
-type commonExpression1Type() :: '+' | '-' | '~' | '++' | '--' | '!' | '*' | '&' | sizeof.

-record(commonExpression2,
        {operator = '+' :: commonExpression2Type(),
         operand1 = [] :: cAST(),
         operand2 = [] :: cAST(),
         line = 0:: lineNumber()}).

-record(commonExpression1,
        {operator = '+' :: commonExpression1Type(),
         operand = [] :: cAST(),
         line = 0:: lineNumber()}).

-record(invocationExpression,
        {function :: cExpression(),
         arguments = [] :: cAST(), %% todo
         line = 0 :: lineNumber()}).

-record(variableReference,
        {name :: atom(),
         line = 0 :: lineNumber()}).

-record(variableDefinition,
        {name :: atom(),
         type :: cType(),
         initialValue :: cExpression() | undefined,
         line = 0 :: lineNumber()}).

-record(functionDefinitionRaw,
        {name :: atom(),
         parameters = [] :: [#variableDefinition{}],
         returnType :: cType(),
         body = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(functionDefinition,
        {name :: atom(),
         parameters = [] :: [#variableDefinition{}],
         returnType :: cType(),
         body = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(structDefinitionRaw,
        {name = '<anonymous>' :: atom(),
         fields = [] :: [#variableDefinition{}],
         line = 0 :: lineNumber()}).

-record(structDefinition,
        {name :: atom(),
         size = 0 :: non_neg_integer(),
         fields = [] :: [#variableDefinition{}],
         line = 0 :: lineNumber()}).

-record(unionDefinitionRaw,
        {name = '<anonymous>' :: atom(),
         fields = [] :: [#variableDefinition{}],
         line = 0 :: lineNumber()}).

-record(unionDefinition,
        {name :: atom(),
         size = 0 :: non_neg_integer(),
         fields = [] :: [#variableDefinition{}],
         line = 0 :: lineNumber()}).

-record(enumDefinitionRaw,
        {name :: atom(),
         variants = [] :: [atom()],
         line = 0 :: lineNumber()}).

-record(enumDefinition,
        {name :: atom(),
         variants = [] :: [atom()],
         line = 0 :: lineNumber()}).

-type typeFlag() :: const | static | extern | volatile | auto | register.

-record(basicType,
        {pointerDepth = 0 :: non_neg_integer(),
         class = integer :: struct | union | enum | integer | float | void | '<unknown>',
         %% this field can be type tag or struct/union/enum name
         tag = u8 :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | f32 | f64 | void | atom(),
         flags = [] :: [typeFlag()],
         line = 0 :: lineNumber()}).

-record(arrayType,
        {elementType :: cType(),
         size = 0 :: non_neg_integer(),
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
         statements = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(doWhileStatement,
        {condition = [] :: cExpression(),
         statements = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(forStatement,
        {initialize = [] :: cExpression(), %% todo: variable definition is also allowed here
         condition = [] :: cExpression(),
         tail = [] :: cExpression(),
         statements = [] :: cAST(),
         line = 0 :: lineNumber()}).

-record(returnStatement,
        {expression :: cExpression(),
         line = 0 :: lineNumber()}).

-record(gotoStatement,
        {expression :: cExpression(),
         line = 0 :: lineNumber()}).

-record(expressionStatement,
        {expression :: cExpression(),
         line = 0 :: lineNumber()}).

-record(gotoLabel,
        {name :: cExpression(),
         line = 0 :: lineNumber()}).

-record(sizeofExpression,
        {type :: cType(),
         line = 0 :: lineNumber()}).

-record(arrayInitializeExpression,
        {elements = [] :: cExpression(),
         line = 0 :: lineNumber()}).

-record(structInitializeExpression,
        {elements = [] :: cExpression(),
         line = 0 :: lineNumber()}).

-type cExpression() :: #commonExpression2{} | #commonExpression1{} | #invocationExpression{} | #sizeofExpression{} |
                       #arrayInitializeExpression{} | #structInitializeExpression{}.
-type cStatement() :: #ifStatement{} | #switchStatement{} | #whileStatement{} | #doWhileStatement{} | #expressionStatement{}.
-type cAST() :: [cStatement()].
-type cType() :: #basicType{} | #arrayType{}.

-endif.

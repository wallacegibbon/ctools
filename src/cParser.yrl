Nonterminals

rootLevelStatementList functionLevelStatementList rootLevelStatement functionLevelStatement statementInCommon statementOnlyInFunction statementOnlyOutsideFunction
structDefinition unionDefinition enumDefinition variableDefinitionList variableDefinition functionDefinition
parameters expressionsSeparatedByComma expression invocationExpression preMinusPlusExpression
ifStatement switchStatement whileStatement doWhileStatement forStatement expressionStatement blockStatement
returnStatement sizeofExpression assignExpression gotoLabel gotoStatement op19 op30 op29 op28 op27 op26 op25 op2WithAssignment
typeAnnotationList typeAnnotation
pointerDepth atomicLiteralValue
arrayInitExpression structInitExpression

.

Terminals

%% the 32 keywords in C language
'if' else switch 'case' default for while do break continue return goto struct enum union sizeof typedef
const static extern volatile auto register signed unsigned char short long int double float void

%% operators
'{' '}' '(' ')' '[' ']' ';' ',' '?' ':' '=' '+' '-' '*' '/' '%' '.' '->'
'!=' '==' '>=' '<='  '<' '>' '>>' '<<' '&' '|' '^' '~' '!' '&&' '||'

%%
cIdentifier cInteger cFloat cCharacter cString

.

Rootsymbol rootLevelStatementList.

%%-------------------------------------------------------------------------------------------------
%% statements and expressions
%%-------------------------------------------------------------------------------------------------

rootLevelStatementList -> rootLevelStatement rootLevelStatementList : ['$1' | '$2'].
rootLevelStatementList -> rootLevelStatement : ['$1'].

functionLevelStatementList -> statementInCommon functionLevelStatementList : ['$1' | '$2'].
functionLevelStatementList -> statementInCommon : ['$1'].

%% for C language, function is only allowed to be defined in up-most level.
rootLevelStatement -> statementInCommon : '$1'.
rootLevelStatement -> statementOnlyOutsideFunction : '$1'.

functionLevelStatement -> statementInCommon : '$1'.
functionLevelStatement -> statementOnlyInFunction : '$1'.

statementOnlyOutsideFunction -> functionDefinition : '$1'.

statementOnlyInFunction -> ifStatement : '$1'.
statementOnlyInFunction -> switchStatement : '$1'.
statementOnlyInFunction -> whileStatement : '$1'.
statementOnlyInFunction -> doWhileStatement : '$1'.
statementOnlyInFunction -> forStatement : '$1'.
statementOnlyInFunction -> expressionStatement ';' : '$1'.
statementOnlyInFunction -> assignExpression ';' : '$1'.
statementOnlyInFunction -> returnStatement ';' : '$1'.
statementOnlyInFunction -> gotoStatement ';' : '$1'.
statementOnlyInFunction -> gotoLabel : '$1'.
statementOnlyInFunction -> blockStatement : '$1'.

statementInCommon -> structDefinition : '$1'.
statementInCommon -> enumDefinition : '$1'.
statementInCommon -> unionDefinition : '$1'.
statementInCommon -> variableDefinition ';' : '$1'.

expressionStatement -> expression ';' : '$1'.

whileStatement -> while '(' expression ')' functionLevelStatementList :
    #whileStatement{condition = '$2', statements = '$4', line = tokenLine('$1')}.

doWhileStatement -> do functionLevelStatementList while '(' expression ')' :
    #doWhileStatement{condition = '$5', statements = '$2', line = tokenLine('$1')}.

ifStatement -> 'if' '(' expression ')' functionLevelStatementList else functionLevelStatementList :
    #ifStatement{condition = '$3', then = '$5', else = '$7', line = tokenLine('$1')}.
ifStatement -> 'if' '(' expression ')' functionLevelStatementList :
    #ifStatement{condition = '$3', then = '$5', line = tokenLine('$1')}.

blockStatement -> '{' functionLevelStatementList '}' :
    '$2'.

gotoLabel -> cIdentifier ':' :
    #gotoLabel{name = tokenValue('$1'), line = tokenLine('$1')}.

returnStatement -> return expression :
    #returnStatement{expression = '$2', line = tokenLine('$1')}.

gotoStatement -> goto expression :
    #gotoStatement{expression = '$2', line = tokenLine('$1')}.

sizeofExpression -> sizeof '(' typeAnnotation ')' :
    #sizeofExpression{type = '$3', line = tokenLine('$1')}.
sizeofExpression -> sizeof typeAnnotation :
    #sizeofExpression{type = '$2', line = tokenLine('$1')}.
sizeofExpression -> sizeof expression :
    #sizeofExpression{type = '$2', line = tokenLine('$1')}.

%% function invocation
invocationExpression -> expression '(' expressionsSeparatedByComma ')' :
    #invocationExpression{function = '$1', arguments = '$3', line = tokenLine('$1')}.
invocationExpression -> expression '(' ')' :
    #invocationExpression{function = '$1', arguments = [], line = tokenLine('$1')}.

expressionsSeparatedByComma -> expression ',' expressionsSeparatedByComma :
    ['$1' | '$3'].
expressionsSeparatedByComma -> expression :
    [$1].

expression -> expression op30 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line=tokenLine('$2')}.
expression -> expression op29 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line=tokenLine('$2')}.
expression -> expression op28 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op27 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op26 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op25 expression :
    #commonExpression2{operator = tokenSymbol('$2'), operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.
expression -> expression op19 :
    #commonExpression1{operator = tokenSymbol('$2'), operand = '$1', line = tokenLine('$2')}.
expression -> cIdentifier :
    #variableReference{name = tokenValue('$1'), line = tokenLine('$1')}.
expression -> preMinusPlusExpression :
    '$1'.
expression -> arrayInitExpression :
    '$1'.
expression -> structInitExpression :
    '$1'.
expression -> atomicLiteralValue :
    '$1'.
expression -> invocationExpression :
    '$1'.
expression -> sizeofExpression :
    '$1'.
expression -> '(' expression ')' :
    '$2'.

pointerDepth -> '*' pointerDepth : '$2' + 1.
pointerDepth -> '*' : 1.

atomicLiteralValue -> cInteger : '$1'.
atomicLiteralValue -> cFloat : '$1'.
atomicLiteralValue -> cString : '$1'.

%% the precedence of 'preMinusPlusExpression' needs to be higher than "op2 -"
Unary 300 preMinusPlusExpression.
preMinusPlusExpression -> '-' expression :
    #commonExpression1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.
preMinusPlusExpression -> '+' expression :
    #commonExpression1{operator = tokenSymbol('$1'), operand = '$2', line = tokenLine('$1')}.

assignExpression -> expression op2WithAssignment expression :
    #commonExpression2{line = tokenLine('$2'), operator = assign, operand1 = '$1',
                       operand2 = #commonExpression2{operator = tokenSymbol('$2'), op1 = '$1', op2 = '$3', line = tokenLine('$2')}}.
assignExpression -> expression '=' expression :
    #commonExpression2{operator = assign, operand1 = '$1', operand2 = '$3', line = tokenLine('$2')}.

op2WithAssignment -> op29 '=' : '$1'.
op2WithAssignment -> op28 '=' : '$1'.
op2WithAssignment -> op27 '=' : '$1'.

Unary 900 op19.
op19 -> '*' : '$1'.
op19 -> '&' : '$1'.
op19 -> '!' : '$1'.
op19 -> '~' : '$1'.

Left 1000 op30.
op30 -> '.' : '$1'.
op30 -> '->' : '$1'.

Left 290 op29.
op29 -> '*' : '$1'.
op29 -> '/' : '$1'.
op29 -> '%' : '$1'.

Left 280 op28.
op28 -> '+' : '$1'.
op28 -> '-' : '$1'.

Left 270 op27.
op27 -> '<<' : '$1'.
op27 -> '>>' : '$1'.
op27 -> '&' : '$1'.
op27 -> '|' : '$1'.
op27 -> '^' : '$1'.

Nonassoc 260 op26.
op26 -> '==' : '$1'.
op26 -> '!=' : '$1'.
op26 -> '>=' : '$1'.
op26 -> '<=' : '$1'.
op26 -> '>' : '$1'.
op26 -> '<' : '$1'.

Left 250 op25.
op25 -> '&&' : '$1'.
op25 -> '||' : '$1'.

%%-------------------------------------------------------------------------------------------------
%% function, struct, union, enum definitions
%%-------------------------------------------------------------------------------------------------
%% struct definition
structDefinition -> struct cIdentifier '{' variableDefinitionList '}' :
    #structDefinitionRaw{name = tokenValue('$2'), fields = '$4', line = tokenLine('$1')}.

%% union definition
unionDefinition -> union cIdentifier '{' variableDefinitionList '}' :
    #unionDefinitionRaw{name = tokenValue('$2'), fields = '$4', line = tokenLine('$1')}.

%% enum definition
enumDefinition -> enum cIdentifier '{' variableDefinitionList '}' :
    #enumDefinitionRaw{name = tokenValue('$2'), variants = '$4', line = tokenLine('$1')}.

functionDefinition -> cIdentifier : '$1'.

%%-------------------------------------------------------------------------------------------------
%% variable definitions
%%-------------------------------------------------------------------------------------------------
variableDefinitionList -> variableDefinition ',' variableDefinitionList :
    ['$1' | '$3'].
variableDefinitionList -> variableDefinition :
    ['$1'].

variableDefinition -> cIdentifier : '$1'.

%%-------------------------------------------------------------------------------------------------
%% initialize expressions for array, struct
%%-------------------------------------------------------------------------------------------------
%% arrayInitExpression and structInitExpression contains similar pattern '{' '}'.
%% make the precedence of arrayInitExpression higher than structInitExpression
Unary 2100 arrayInitExpression.
arrayInitExpression -> '{' expressionsSeparatedByComma '}' :
    #arrayInitializeExpression{elements = '$2', line = tokenLine('$1')}.
arrayInitExpression -> '{' cString '}' :
    #arrayInitializeExpression{elements = stringToIntegerTokens('$2'), line = tokenLine('$1')}.

Unary 2000 structInitExpression.
structInitExpression -> cIdentifier '{' expressionsSeparatedByComma '}' :
    #structInitializeExpression{name = tokenValue('$1'), fields = '$3', line = tokenLine('$1')}.

Erlang code.

-include("./cScanner.hrl").
-include("./cAST.hrl").

-spec stringToIntegerTokens(cString()) -> [cInteger()].
stringToIntegerTokens({string, LineNumber, BinaryString}) ->
    [{cInteger, LineNumber, Number} || <<Number>> <= BinaryString].

tokenValue({_, _, Value}) ->
    Value.

tokenSymbol({Symbol, _}) ->
    Symbol.

tokenLine(T) ->
    element(2, T).

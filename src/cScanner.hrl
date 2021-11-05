-type lineNumber() :: pos_integer().

-type cOperator() :: {Tag :: atom(), lineNumber()}.
-type cFloat() :: {float, lineNumber(), Value :: float()}.
-type cInteger() :: {integer, lineNumber(), Value :: integer()}.
-type cCharacter() :: {character, lineNumber(), Value :: integer()}.
-type cString() :: {string, lineNumber(), Value :: binary()}.
-type cLineComment() :: {lineComment, lineNumber(), Value :: binary()}.
-type cBlockComment() :: {blockComment, lineNumber(), Value :: binary()}.
-type cIdentifier() :: {identifier, lineNumber(), Value :: atom()}.

-type token() :: cOperator() | cFloat() | cInteger() | cCharacter() | cString() | cIdentifier() | cLineComment() | cBlockComment().

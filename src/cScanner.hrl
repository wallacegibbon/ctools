-ifndef(__cScanner_hrl__).
-define(__cScanner_hrl__, true).

-type lineNumber() :: pos_integer().

-type cOperator() :: {Tag :: atom(), lineNumber()}.
-type cFloat() :: {cFloat, lineNumber(), Value :: float()}.
-type cInteger() :: {cInteger, lineNumber(), Value :: integer()}.
-type cCharacter() :: {cCharacter, lineNumber(), Value :: integer()}.
-type cString() :: {cString, lineNumber(), Value :: binary()}.
-type cLineComment() :: {cLineComment, lineNumber(), Value :: binary()}.
-type cBlockComment() :: {cBlockComment, lineNumber(), Value :: binary()}.
-type cIdentifier() :: {cIdentifier, lineNumber(), Value :: atom()}.

-type token() :: cOperator() | cFloat() | cInteger() | cCharacter() | cString() | cIdentifier() | cLineComment() | cBlockComment().

-endif.

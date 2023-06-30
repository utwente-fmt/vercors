lexer grammar LangCPPLexer;
import SpecLexer;

@lexer::members {
    private static boolean inBlockSpec = false;
    private static boolean inLineSpec = false;
}

channels {
  EXPECTED_ERROR_CHANNEL,
  LINE_DIRECTIVE_CHANNEL
}

// lexer tokens needed for the SpecParser
VAL_INLINE: EOF EOF;
VAL_ASSERT: 'assert';
VAL_PACKAGE: 'package';
VAL_BOOL: EOF EOF;

IntegerLiteral:
	DecimalLiteral Integersuffix?
	| OctalLiteral Integersuffix?
	| HexadecimalLiteral Integersuffix?
	| BinaryLiteral Integersuffix?;

CharacterLiteral:
	('u' | 'U' | 'L')? '\'' Cchar+ '\'';

FloatingLiteral:
	Fractionalconstant Exponentpart? Floatingsuffix?
	| Digitsequence Exponentpart Floatingsuffix?;

StringLiteral:
	Encodingprefix?
    (Rawstring
	|'"' Schar* '"');

BooleanLiteral: False | True;

PointerLiteral: Nullptr;

UserDefinedLiteral:
	UserDefinedIntegerLiteral
	| UserDefinedFloatingLiteral
	| UserDefinedStringLiteral
	| UserDefinedCharacterLiteral;

MultiLineMacro:
	'#' (~[\n]*? '\\' '\r'? '\n')+ ~ [\n]+ -> channel (LINE_DIRECTIVE_CHANNEL);

Directive: '#' ~ [\n]* -> channel (LINE_DIRECTIVE_CHANNEL);

Alignas: 'alignas';
Alignof: 'alignof';
Asm: 'asm';
Auto: 'auto';
Bool: 'bool';
Break: 'break';
Case: 'case';
Catch: 'catch';
Char: 'char';
Char16: 'char16_t';
Char32: 'char32_t';
Class: 'class';
Const: 'const';
Constexpr: 'constexpr';
Const_cast: 'const_cast';
Continue: 'continue';
Decltype: 'decltype';
Default: 'default';
Delete: 'delete';
Do: 'do';
Double: 'double';
Dynamic_cast: 'dynamic_cast';
Else: 'else';
Enum: 'enum';
Explicit: 'explicit';
Export: 'export';
Extern: 'extern';
False: 'false';
Final: 'final';
Float: 'float';
For: 'for';
Friend: 'friend';
Goto: 'goto';
If: 'if';
Inline: 'inline';
Int: 'int';
Long: 'long';
Mutable: 'mutable';
Namespace: 'namespace';
New: 'new';
Noexcept: 'noexcept';
Nullptr: 'nullptr';
Operator: 'operator';
Override: 'override';
Private: 'private';
Protected: 'protected';
Public: 'public';
Register: 'register';
Reinterpret_cast: 'reinterpret_cast';
Return: 'return';
Short: 'short';
Signed: 'signed';
Sizeof: 'sizeof';
Static: 'static';
Static_assert: 'static_assert';
Static_cast: 'static_cast';
Struct: 'struct';
Switch: 'switch';
Template: 'template';
This: 'this';
Thread_local: '_thread_local';
Throw: 'throw';
True: 'true';
Try: 'try';
Typedef: 'typedef';
Typeid_: 'typeid';
Typename_: 'typename';
Union: 'union';
Unsigned: 'unsigned';
Using: 'using';
Virtual: 'virtual';
Void: 'void';
Volatile: 'volatile';
Wchar: 'wchar_t';
While: 'while';

LeftParen: '(';
RightParen: ')';
LeftBracket: '[';
RightBracket: ']';
LeftBrace: '{';
RightBrace: '}';
Plus: '+';
Minus: '-';
Star: '*';
Div: '/';
Mod: '%';
Caret: '^';
And: '&';
Or: '|';
Tilde: '~';
Not: '!';
NotWord: 'not';
Assign: '=';
Less: '<';
Greater: '>';
PlusAssign: '+=';
MinusAssign: '-=';
StarAssign: '*=';
DivAssign: '/=';
ModAssign: '%=';
XorAssign: '^=';
AndAssign: '&=';
OrAssign: '|=';
LeftShiftAssign: '<<=';
RightShiftAssign: '>>=';
Equal: '==';
NotEqual: '!=';
LessEqual: '<=';
GreaterEqual: '>=';
AndAnd: '&&' | 'and';
OrOr: '||' | 'or';
PlusPlus: '++';
MinusMinus: '--';
Comma: ',';
ArrowStar: '->*';
Arrow: '->';
Question: '?';
Colon: ':';
Doublecolon: '::';
Semi: ';';
Dot: '.';
DotStar: '.*';
Ellipsis: '...';
fragment Hexquad:
	HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT;

fragment Universalcharactername:
	'\\u' Hexquad
	| '\\U' Hexquad Hexquad;

fragment Identifiernondigit: NONDIGIT | Universalcharactername;

fragment NONDIGIT: [a-zA-Z_];

fragment DIGIT: [0-9];

DecimalLiteral: NONZERODIGIT ('\''? DIGIT)*;

OctalLiteral: '0' ('\''? OCTALDIGIT)*;

HexadecimalLiteral: ('0x' | '0X') HEXADECIMALDIGIT (
		'\''? HEXADECIMALDIGIT
	)*;

BinaryLiteral: ('0b' | '0B') BINARYDIGIT ('\''? BINARYDIGIT)*;

fragment NONZERODIGIT: [1-9];

fragment OCTALDIGIT: [0-7];

fragment HEXADECIMALDIGIT: [0-9a-fA-F];

fragment BINARYDIGIT: [01];

Integersuffix:
	Unsignedsuffix Longsuffix?
	| Unsignedsuffix Longlongsuffix?
	| Longsuffix Unsignedsuffix?
	| Longlongsuffix Unsignedsuffix?;

fragment Unsignedsuffix: [uU];

fragment Longsuffix: [lL];

fragment Longlongsuffix: 'll' | 'LL';

fragment Cchar:
	~ ['\\\r\n]
	| Escapesequence
	| Universalcharactername;

fragment Escapesequence:
	Simpleescapesequence
	| Octalescapesequence
	| Hexadecimalescapesequence;

fragment Simpleescapesequence:
	'\\\''
	| '\\"'
	| '\\?'
	| '\\\\'
	| '\\a'
	| '\\b'
	| '\\f'
	| '\\n'
	| '\\r'
	| ('\\' ('\r' '\n'? | '\n'))
	| '\\t'
	| '\\v';

fragment Octalescapesequence:
	'\\' OCTALDIGIT
	| '\\' OCTALDIGIT OCTALDIGIT
	| '\\' OCTALDIGIT OCTALDIGIT OCTALDIGIT;

fragment Hexadecimalescapesequence: '\\x' HEXADECIMALDIGIT+;

fragment Fractionalconstant:
	Digitsequence? '.' Digitsequence
	| Digitsequence '.';

fragment Exponentpart:
	'e' SIGN? Digitsequence
	| 'E' SIGN? Digitsequence;

fragment SIGN: [+-];

fragment Digitsequence: DIGIT ('\''? DIGIT)*;

fragment Floatingsuffix: [flFL];

fragment Encodingprefix: 'u8' | 'u' | 'U' | 'L';

fragment Schar:
	~ ["\\\r\n]
	| Escapesequence
	| Universalcharactername;

fragment Rawstring: 'R"' (( '\\' ["()] )|~[\r\n (])*? '(' ~[)]*? ')'  (( '\\' ["()]) | ~[\r\n "])*? '"';

UserDefinedIntegerLiteral:
	DecimalLiteral Udsuffix
	| OctalLiteral Udsuffix
	| HexadecimalLiteral Udsuffix
	| BinaryLiteral Udsuffix;

UserDefinedFloatingLiteral:
	Fractionalconstant Exponentpart? Udsuffix
	| Digitsequence Exponentpart Udsuffix;

UserDefinedStringLiteral: StringLiteral Udsuffix;

UserDefinedCharacterLiteral: CharacterLiteral Udsuffix;

fragment Udsuffix: Identifier;

BlockStartSpecImmediate: '/*' [ \t\u000C]* '@' {inBlockSpec = true;};

BlockCommentStart: '/*' -> mode(COMMENT), skip;

LineCommentStart: '//' -> mode(LINE_COMMENT), skip;

EndSpec:
    '@'? '*/' {inBlockSpec}? {inBlockSpec = false;}
    | ('\n'|'\r\n') {inLineSpec}? {inLineSpec = false;};

Whitespace: [ \t]+ -> skip;

Newline: ('\r' '\n'? | '\n') -> skip;


mode DEFAULT_MODE;
Identifier: Identifiernondigit (Identifiernondigit | DIGIT)*;

ExtraAt: ('\n'|'\r\n') [ \t\u000C]* '@' {inBlockSpec}? -> skip;

mode COMMENT;
BlockCommentStop: '*/' -> mode(DEFAULT_MODE), skip;
BlockStartSpec: ('\n'|'\r\n') [ \t\u000C]* '@' {inBlockSpec = true;} -> mode(DEFAULT_MODE);
BlockCommentContent: .+? -> skip;

mode LINE_COMMENT;
LineCommentStop: ('\n'|'\r\n') -> mode(DEFAULT_MODE), skip;
LineStartSpec: '@' {inLineSpec = true;} -> mode(DEFAULT_MODE);
LineCommentContent: .+? -> skip;
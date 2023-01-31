lexer grammar LangPVLLexer;
import SpecLexer;

channels {
  EXPECTED_ERROR_CHANNEL
}

VAL_INLINE: 'inline';
VAL_ASSERT: 'assert';
VAL_PACKAGE: 'package';

PAREN_OPEN: '(';
PAREN_CLOSE: ')';
BLOCK_OPEN: '{';
BLOCK_CLOSE: '}';
ANGLE_OPEN: '<';
ANGLE_CLOSE: '>';
BRACK_OPEN: '[';
BRACK_CLOSE: ']';
ARR_RIGHT: '->';

COMMA: ',';
POINT: '.';
COLON: ':';
SEMICOLON: ';';

QUESTION: '?';
EXCL: '!';
AND: '&&';
OR: '||';
BITOR: '|';
XOR: '^^';

GTE: '>=';
LTE: '<=';
ASSIGN: '=';
EQ: '==';
INEQ: '!=';

PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
PERCENT: '%';
INC: '++';
DEC: '--';
CONS: '::';

ENUM: 'enum';
CLASS: 'class';
KERNEL: 'kernel';
BARRIER: 'barrier';
INVARIANT: 'invariant';
CONSTRUCTOR: 'constructor';
RUN: 'run';

IF: 'if';
ELSE: 'else';
WHILE: 'while';
FOR: 'for';
GOTO: 'goto';
RETURN: 'return';
VEC: 'vec';
PAR: 'par';
PAR_AND: 'and';
PARALLEL: 'parallel';
SEQUENTIAL: 'sequential';
BLOCK: 'block';

LOCK: 'lock';
UNLOCK: 'unlock';
WAIT: 'wait';
NOTIFY: 'notify';
FORK: 'fork';
JOIN: 'join';

THIS: 'this';
NULL: 'null';
TRUE: 'true';
FALSE: 'false';
CURRENT_THREAD: 'current_thread';
CURRENT_THREAD_ESC: '\\current_thread';
OWNER: '\\owner';

GLOBAL: 'global';
LOCAL: 'local';
STATIC: 'static';
UNFOLDING_ESC: '\\unfolding';
UNFOLDING: 'unfolding';
IN_ESC: '\\in';
IN: 'in';
NEW: 'new';
ID: 'id';

BOOL: 'boolean';
VOID: 'void';
INT: 'int';
CHAR: 'char';
FLOAT32: 'float32';
FLOAT64: 'float64';
STRING_CLASS: 'String';

NUMBER : ('0'..'9')+;
DECIMAL_NUMBER : ('0'..'9')+ '.' ('0'..'9')+;
DECIMAL_NUMBER_F : ('0'..'9')+ '.' ('0'..'9')+ 'f';

CHARACTER_LITERAL : '\'' CHARACTER '\'';

fragment
CHARACTER
    : '\\' '\''
    | ~['\\]
    ;

STRING_LITERAL : '"' STRING_CHARACTER* '"';

fragment
STRING_CHARACTER
    : ~["\\ | 'String']
    | ESCAPE
    ;

fragment
ESCAPE
    :   '\\' [tnr"'\\]
    | UNICODE_ESCAPE
    ;

fragment
UNICODE_ESCAPE
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

fragment
HEX_DIGIT
    :   [0-9a-fA-F]
    ;

mode DEFAULT_MODE;
Identifier  : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;

COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT : '//' .*? ('\n'|EOF) -> skip;

WS  :   (   ' '
        |   '\t'
        |   '\r'
        |   '\n'
        )+ -> skip ;

EmbeddedLatex
    : '#' ~[\r\n]* '#' -> skip
    ;
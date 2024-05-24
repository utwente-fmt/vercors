lexer grammar LangPVLLexer;
import SpecLexer;

channels {
  EXPECTED_ERROR_CHANNEL
}

VAL_INLINE: {"keyword.other";} 'inline';
VAL_ASSERT: {"keyword.other";} 'assert';
VAL_PACKAGE: {"meta.*"} 'package';

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

QUESTION: {"keyword.operator";} '?';
EXCL: {"keyword.operator";} '!';
AND: {"keyword.operator";} '&&';
OR: {"keyword.operator";} '||';
BITOR: {"keyword.operator";} '|';
XOR: {"keyword.operator";} '^^';

GTE: {"keyword.operator";} '>=';
LTE: {"keyword.operator";} '<=';
ASSIGN: {"keyword.operator";} '=';
EQ: {"keyword.operator";} '==';
INEQ: {"keyword.operator";} '!=';

PLUS: {"keyword.operator";} '+';
MINUS: {"keyword.operator";} '-';
STAR: {"keyword.operator";} '*';
SLASH: {"keyword.operator";} '/';
PERCENT: {"keyword.operator";} '%';
INC: {"keyword.operator";} '++';
DEC: {"keyword.operator";} '--';
CONS: {"keyword.operator";} '::';

ENUM: {"storage.type";} 'enum';
CLASS: {"storage.type";} 'class';
SEQ_PROGRAM: {"keyword.control.concurrency";} 'seq_program';
SEQ_RUN: 'seq_run';
KERNEL: {"keyword.control.concurrency";} 'kernel';
BARRIER: {"keyword.control.concurrency";} 'barrier';
INVARIANT: {"keyword.control.concurrency";} 'invariant';
CONSTRUCTOR: {"variable.language";} 'constructor';
RUN: {"keyword.control.concurrency";} 'run';
VESUV_ENTRY: 'vesuv_entry';
THREAD: {"keyword.control.concurrency";} 'thread';
ENDPOINT: 'endpoint';

IF: {"keyword.control";} 'if';
ELSE: {"keyword.control";} 'else';
WHILE: {"keyword.control";} 'while';
FOR: {"keyword.control";} 'for';
GOTO: {"keyword.control";} 'goto';
RETURN: {"keyword.control";} 'return';
VEC: {"variable.language";} 'vec';
PAR: {"keyword.control.concurrency";} 'par';
PAR_AND: {"keyword.control.concurrency";} 'and';
PARALLEL: {"keyword.control.concurrency";} 'parallel';
SEQUENTIAL: {"keyword.control.concurrency";} 'sequential';
BLOCK: {"keyword.control";} 'block';

LOCK: {"keyword.control.concurrency";} 'lock';
UNLOCK: {"keyword.control.concurrency";} 'unlock';
WAIT: {"keyword.control.concurrency";} 'wait';
NOTIFY: {"keyword.control.concurrency";} 'notify';
FORK: {"keyword.control.concurrency";} 'fork';
JOIN: {"keyword.control.concurrency";} 'join';

THIS: {"variable.language";} 'this';
NULL: {"variable.language";} 'null';
TRUE: {"variable.language";} 'true';
FALSE: {"variable.language";} 'false';
CURRENT_THREAD: {"variable.language";} 'current_thread';
CURRENT_THREAD_ESC: {"variable.language";} '\\current_thread';
OWNER: {"variable.language";} '\\owner';

GLOBAL: {"storage.modifier";} 'global';
LOCAL: {"storage.modifier";} 'local';
STATIC: {"storage.modifier";} 'static';
FINAL: {"storage.modifier";} 'final';
UNFOLDING_ESC: {"variable.language";} '\\unfolding';
UNFOLDING: {"variable.language";} 'unfolding';
IN_ESC: {"variable.language";} '\\in';
IN: {"variable.language";} 'in';
NEW: {"variable.language";} 'new';
ID: {"variable.language";} 'id';

BOOL: {"keyword.type";} 'boolean';
VOID: {"keyword.type";} 'void';
INT: {"keyword.type";} 'int';
CHAR: {"keyword.type";} 'char';
FLOAT32: {"keyword.type";} 'float32';
FLOAT64: {"keyword.type";} 'float64';

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
    : ~["\\]
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
Identifier : {"entity.name";} ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;

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
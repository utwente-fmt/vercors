lexer grammar LangPVLLexer;
import SpecLexer;

VAL_INLINE: 'inline';
VAL_ASSERT: 'assert';

PAREN_OPEN: '(';
PAREN_CLOSE: ')';
BLOCK_OPEN: '{';
BLOCK_CLOSE: '}';
ANGLE_OPEN: '<';
ANGLE_CLOSE: '>';
BRACK_OPEN: '[';
BRACK_CLOSE: ']';
ARR_RIGHT: '->';
ARR_LEFT: '<-';
EMPTY_SEQ_OPEN: '[t:';
EMPTY_SET_OPEN: '{t:';
BAG_OPEN: 'b{';
EMPTY_BAG_OPEN: 'b{t:';
HOARE_PRED_OPEN: '{*';
HOARE_PRED_CLOSE: '*}';

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

CLASS: 'class';
KERNEL: 'kernel';
BARRIER: 'barrier';
INVARIANT: 'invariant';

IF: 'if';
ELSE: 'else';
WHILE: 'while';
FOR: 'for';
GOTO: 'goto';
RETURN: 'return';
VEC: 'vec';
PAR: 'par';
PAR_AND: 'and';

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

GPUOPT: 'gpuopt';
GPUOPT_LOOPUNROLL: 'loop_unroll';
GPUOPT_MATRIX_REPR: 'matrix_lin';
GPUOPT_ITER_MERGE: 'iter_merge';
GPUOPT_GLOB_TO_REG: 'glob_to_reg';
GPUOPT_TILE: 'tile';
GPUOPT_FUSE: 'fuse';
COLUMN_MAJOR: 'C';
ROW_MAJOR: 'R';
TILE_INTER: 'inter';
TILE_INTRA: 'intra';

MAP: 'map';
TUPLE: 'tuple';
BOOL: 'boolean';
VOID: 'void';
INT: 'int';
STRING: 'string';
OPTION: 'option';

NUMBER : ('0'..'9')+;

mode DEFAULT_MODE;
Identifier  : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;

COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT : '//' .*? '\n' -> skip;

WS  :   (   ' '
        |   '\t'
        |   '\r'
        |   '\n'
        )+ -> skip ;

EmbeddedLatex
    : '#' ~[\r\n]* '#' -> skip
    ;
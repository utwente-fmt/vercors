lexer grammar LangLLVMSpecLexer;
import SpecLexer;

@lexer::members {
    private static boolean inBlockSpec = false;
    private static boolean inLineSpec = false;
}

channels {
  EXPECTED_ERROR_CHANNEL
}
// mandatory tokens
SEMI: ';';
BLOCK_OPEN: '{';
BLOCK_CLOSE: '}';
BRACK_OPEN: '[';
BRACK_CLOSE: ']';
ANGLE_OPEN: '<';
ANGLE_CLOSE: '>';
EQ: '=';
EQUALS: '==';
EXCL: '!';
STAR: '*';
PIPE: '|';
PLUS: '+';
COLON: ':';
VAL_INLINE: 'inline';
VAL_ASSERT: 'assert';
VAL_PACKAGE: 'package';

INC: '++';
ARROW: '->';
DOT: '.';


// misc characters
Percent : '%';
AtSign : '@';
Sign    : '-';
Lparen  : '(';
Rparen  : ')';
Comma   : ',';

// useless stuff but neccesary i guess
StartSpec: '"}';
EndSpec: '!VC.contract !{"';

// identifiers

fragment Digit: [0-9];

Identifier: NamedIdentifier | UnnamedIdentifier;

NamedIdentifier: [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*;

UnnamedIdentifier: [%@]Digit+;

True: 'true';
False: 'false';

IntegerConstant : Sign?Digit+;

Whitespace
    :   [ \t]+
        -> skip
    ;
Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;

// operators
// operators -> binops
ADD: 'add';
SUB: 'sub';
MUL: 'mul';
UDIV: 'udiv';
SDIV: 'sdiv';

// operators -> other
ICMP: 'icmp';

// compare predicates
EQ_pred: 'eq';
NE_pred: 'ne';
UGT_pred: 'ugt';
UGE_pred: 'uge';
ULT_pred: 'ult';
ULE_pred: 'ule';
SGT_pred: 'sgt';
SGE_pred: 'sge';
SLT_pred: 'slt';
SLE_pred: 'sle';

// typing
T_int: 'i'[2-9]Digit*;
T_bool: 'i1';
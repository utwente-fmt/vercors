grammar test;

@members {
    private static final int PROGRAM = 0;
    private static final int COMMENT = 1;
    private static final int CONTRACT = 2;
    private int mode = PROGRAM;
}

start
    : program EOF
    ;

program
    : statement*
    ;

statement
    : contract? Statement Semi
    ;

contract
    : contractBlock+
    ;

contractBlock
    : StartContract contractSpec* EndContract
    ;

contractSpec
    : ContractSpec Semi
    ;

Statement : 'return';
Semi: ';';
ContractSpec : 'assert';

StartComment:                    '/*' {mode=COMMENT;} -> skip;

StartContract: {mode==COMMENT}?  '@'  {mode=CONTRACT;};
ExtraAt:       {mode==CONTRACT}? '@' -> skip;
SkipComment:   {mode==COMMENT}?  .+? -> skip;

EndContract:   {mode==CONTRACT}? '*/' {mode=PROGRAM;};
EndComment:    {mode==COMMENT}?  '*/' {mode=PROGRAM;} -> skip;

Whitespace: [\p{White_Space}] -> skip;
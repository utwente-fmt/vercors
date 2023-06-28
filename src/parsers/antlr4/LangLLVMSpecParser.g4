parser grammar LangLLVMSpecParser;

expression
    : instruction
    | constant
    | identifier
    | valExpr
    ;

instruction
    : binOpInstruction # binOpRule
    | compareInstruction # cmpOpRule
    | callInstruction # callOpRule
    ;

constant
    : booleanConstant # boolConstRule
    | integerConstant # intConstRule
    ;

type
    : T_int # t_intRule
    | T_bool # t_boolRule
    ;

booleanConstant
    : True # constTrue
    | False # constFalse
    ;

integerConstant: IntegerConstant;

identifier: Identifier;

expressionList
    : expression
    | expression Comma expressionList
    ;

binOpInstruction: binOp Lparen expression Comma expression Rparen;

compareInstruction: compOp Lparen compPred Comma expression Comma expression Rparen;

callInstruction: CALL Identifier Lparen expressionList Rparen;



binOp
    : ADD # add
    | SUB # sub
    | MUL # mul
    | UDIV # udiv
    | SDIV # sdiv
    ;


compOp
    : ICMP
    ; // FCMP at some point

compPred
    : EQ_pred  # eq
    | NE_pred  # ne
    | UGT_pred # ugt
    | UGE_pred # uge
    | ULT_pred # ult
    | ULE_pred # ule
    | SGT_pred # sgt
    | SGE_pred # sge
    | SLT_pred # slt
    | SLE_pred # sle
    ;


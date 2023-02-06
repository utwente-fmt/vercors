parser grammar LangPVLParser;

@parser::members {
    private static int specLevel = 1;
}

program  : programDecl* EOF EOF ;

programDecl : valGlobalDeclaration | declClass | method | declVeyMontSeqProg;

declClass : contract 'class' identifier '{' classDecl* '}' ;
declVeyMontSeqProg : 'seq_program' identifier '(' args? ')' '{' seqProgDecl* '}';
classDecl : valClassDeclaration | constructor | method | field | runMethod;
seqProgDecl
 : 'thread' identifier '=' type '(' exprList? ')' ';' # seqProgThread
 | runMethod # seqProgRunMethod
 | method # seqProgMethod
 ;

field : type identifierList ';' ;

method : contract valModifier* type identifier '(' args? ')' methodBody ;
methodBody : ';' | block ;

constructor : contract 'constructor' '(' args? ')' methodBody ;

runMethod : contract 'run' methodBody ;

contract : valContractClause* ;

args
 : type identifier
 | type identifier ',' args
 ;

exprList
 : expr
 | expr ',' exprList
 ;

expr
 : valWith? unfoldingExpr valThen?
 ;

unfoldingExpr
 : 'unfolding' unfoldingExpr 'in' unfoldingExpr
 | iteExpr
 ;

iteExpr
 : implicationExpr '?' implicationExpr ':' iteExpr
 | implicationExpr
 ;

implicationExpr
 : orExpr valImpOp implicationExpr
 | orExpr
 ;

orExpr
 : orExpr '||' andExpr
 | andExpr
 ;

andExpr
 : andExpr '&&' eqExpr
 | andExpr valAndOp eqExpr
 | eqExpr
 ;

eqExpr
 : eqExpr '==' relExpr
 | eqExpr '!=' relExpr
 | relExpr
 ;

relExpr
 : relExpr '<' addExpr
 | relExpr '<=' addExpr
 | relExpr '>=' addExpr
 | relExpr '>' addExpr
 | relExpr valInOp addExpr
 | setExpr
 ;

setExpr
 : setExpr 'in' addExpr
 | addExpr
 ;

addExpr
 : addExpr '+' multExpr
 | addExpr '-' multExpr
 | multExpr
 ;

multExpr
 : multExpr '*' powExpr
 | multExpr '/' powExpr
 | multExpr '%' powExpr
 | multExpr valMulOp powExpr
 | powExpr
 ;

powExpr
 : powExpr '^^' seqAddExpr
 | seqAddExpr
 ;

seqAddExpr
 : unaryExpr valPrependOp seqAddExpr
 | seqAddExpr valAppendOp unaryExpr
 | seqAddExpr valAppendOp '(' expr ',' expr ')'
 | unaryExpr
 ;

unaryExpr
 : '!' unaryExpr
 | '-' unaryExpr
 | valPrefix unaryExpr
 | newExpr
 ;

newExpr
 : 'new' classType call
 | 'new' nonArrayType newDims
 | postfixExpr
 ;

postfixExpr
 : postfixExpr '.' identifier call?
 | postfixExpr '[' expr ']'
 | postfixExpr valPostfix
 | unit
 ;

unit
 : valExpr
 | 'this'
 | 'null'
 | NUMBER
 | DECIMAL_NUMBER
 | DECIMAL_NUMBER_F
 | '(' expr ')'
 | identifier call?
 | valGenericAdtInvocation
 ;

call : typeArgs? tuple valGiven? valYields?;
tuple : '(' exprList? ')';

block : '{' statement* '}' ;

statement
 : 'return' expr? ';' # pvlReturn
 | 'lock' expr ';' # pvlLock
 | 'unlock' expr ';' # pvlUnlock
 | 'wait' expr ';' # pvlWait
 | 'notify' expr ';' # pvlNotify
 | 'fork' expr ';' # pvlFork
 | 'join' expr ';' # pvlJoin
 | valStatement # pvlValStatement
 | 'if' '(' expr ')' statement elseBlock? # pvlIf
 | 'barrier' '(' identifier barrierTags? ')' barrierBody # pvlBarrier
 | parRegion # pvlPar
 | 'vec' '(' iter ')' block # pvlVec
 | 'invariant' identifier '(' expr ')' block # pvlInvariant
 | 'atomic' '(' identifierList ')' block # pvlAtomic
 | invariantList 'while' '(' expr ')' statement # pvlWhile
 | invariantList 'for' '(' forStatementList? ';' expr? ';' forStatementList? ')' statement # pvlFor
 | block # pvlBlock
 | 'goto' identifier ';' # pvlGoto
 | 'label' identifier ';' # pvlLabel
 | allowedForStatement ';' # pvlForStatement
 ;

elseBlock: 'else' statement;
barrierTags: ';' identifierList;
barrierBody: '{' contract '}' | contract block;

allowedForStatement
 : type declList # pvlLocal
 | expr # pvlEval
 | identifier ('++'|'--') # pvlIncDec
 | expr '=' expr # pvlAssign
 ;

forStatementList
 : allowedForStatement
 | allowedForStatement ',' forStatementList
 ;

parRegion
 : 'parallel' '{' parRegion* '}' # pvlParallel
 | 'sequential' '{' parRegion* '}' # pvlSequential
 | 'block' identifier? parBlockIter? contract statement # pvlParBlock
 | 'par' parOldUnitList # pvlOldPar
 ;

parOldUnit
 : identifier? parBlockIter? contract statement # pvlOldParUnit
 ;

parOldUnitList
 : parOldUnit
 | parOldUnit 'and' parOldUnitList
 ;


declList
 : identifier declInit?
 | identifier declInit? ',' declList
 ;

declInit : '=' expr ;

parBlockIter: '(' iters ')';
iters: iter | iter ',' iters;
iter: type identifier '=' expr '..' expr;

parWaitList: ';' waitList;
waitList: waitFor | waitFor ',' waitList;
waitFor: identifier waitForArgs?;
waitForArgs: '(' idArgList ')';
idArgList: idArg | idArg ',' idArgList;
idArg: identifier | '*';

invariantList: invariant*;
invariant: 'loop_invariant' expr ';';

nonArrayType
 : valType
 | ('string' | 'int' | 'boolean' | 'void' | 'float32' | 'float64')
 | classType
 ;

type : nonArrayType typeDims? ;
typeList
  : type
  | type ',' typeList
  ;

typeDims
 : quantifiedDim+
 | anonDim+
 ;

newDims : quantifiedDim+ ;
quantifiedDim : '[' expr ']' ;
anonDim : '[' ']' ;
classType : identifier typeArgs?;
typeArgs : '<' typeList '>';

identifierList
 : identifier
 | identifier ',' identifierList
 ;

identifier : Identifier | LANG_ID_ESCAPE ;
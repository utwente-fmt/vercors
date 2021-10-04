parser grammar LangPVLParser;

@parser::members {
    private static int specLevel = 1;
}

program  : programDecl* EOF ;

programDecl : declClass | method | valGlobalDeclaration;

declClass : 'class' identifier '{' classDecl* '}' ;
classDecl : constructor | method | field | valClassDeclaration;

field : type identifierList ';' ;

method : contract valModifier* type identifier '(' args? ')' methodBody ;
methodBody : ';' | block ;

constructor : contract identifier '(' args? ')' methodBody ;

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
 : expr 'with' block
 | expr 'then' block
 | 'unfolding' expr 'in' expr
 | iteExpr
 ;

iteExpr
 : implicationExpr '?' implicationExpr ':' iteExpr
 | implicationExpr
 ;

implicationExpr
 : implicationExpr valImpOp orExpr
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
 | newExpr
 ;

newExpr
 : 'new' identifier tuple
 | 'new' nonArrayType newDims
 | postfixExpr
 ;

postfixExpr
 : postfixExpr '.' identifier
 | postfixExpr '[' expr ']'
 | postfixExpr tuple
 | postfixExpr valPostfix
 | unit
 ;

unit
 : valPrimary
 | 'this'
 | 'null'
 | NUMBER
 | '(' expr ')'
 | identifier
 ;

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
 | contract 'par' parUnitList # pvlPar
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
parUnitList: parUnit | parUnit 'and' parUnitList;

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

declList
 : identifier declInit?
 | identifier declInit? ',' declList
 ;

declInit : '=' expr ;

parUnit
 : identifier? '(' iters? parWaitList? ')' contract block
 | contract block
 ;

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
 : ('string' | 'int' | 'boolean' | 'void')
 | classType
 ;

type : nonArrayType typeDims? ;

typeDims
 : quantifiedDim+
 | anonDim+
 ;

newDims : quantifiedDim+ ;
quantifiedDim : '[' expr ']' ;
anonDim : '[' ']' ;
classType : identifier typeArgs?;
typeArgs : '<' exprList '>';

identifierList
 : identifier
 | identifier ',' identifierList
 ;

identifier : Identifier | valReserved ;
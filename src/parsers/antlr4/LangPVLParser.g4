parser grammar LangPVLParser;

@parser::members {
    private static int specLevel = 1;
}

program  : programDecl* EOF EOF ;

programDecl : valGlobalDeclaration | declClass | enumDecl | method | declVeyMontSeqProg | vesuvEntry ;

enumDecl : 'enum' identifier '{' identifierList? ','? '}' ;

declClass
 : contract 'class' identifier declaredTypeArgs? '{' classDecl* '}'
 ;

declVeyMontSeqProg : contract 'choreography' identifier '(' args? ')' '{' seqProgDecl* '}';

seqProgDecl
 : 'endpoint' identifier '=' classType '(' exprList? ')' ';' # pvlEndpoint
 | contract 'run' block # pvlSeqRun
 | method # seqProgMethod
 ;

applicableReference
 : identifier '.' identifier # pvlAdtFunctionRef
 | identifier                # pvlFunctionRef
 ;

classDecl : valClassDeclaration | constructor | method | field | runMethod;
finalFlag: 'final';
field : finalFlag? type identifierList ';' ;

method : contract valModifier* type identifier declaredTypeArgs? '(' args? ')' methodBody ;
methodBody : ';' | block ;

constructor : contract 'constructor' declaredTypeArgs? '(' args? ')' methodBody ;

runMethod : contract 'run' methodBody ;

vesuvEntry : 'vesuv_entry' methodBody ;

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
 | STRING_LITERAL
 | CHARACTER_LITERAL
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
 | 'communicate' '(' '*' ')' statement elseBlock? # pvlIndetBranch
 | 'if' '(' expr ')' statement elseBlock? # pvlIf
 | 'barrier' '(' identifier barrierTags? ')' barrierBody # pvlBarrier
 | parRegion # pvlPar
 | 'vec' '(' iter ')' block # pvlVec
 | 'invariant' identifier '(' expr ')' block # pvlInvariant
 | 'atomic' '(' identifierList ')' block # pvlAtomic
 | contract 'while' '(' expr ')' statement # pvlWhile
 | contract 'for' '(' forStatementList? ';' expr? ';' forStatementList? ')' statement # pvlFor
 | contract 'for' '(' iter ')' statement # pvlRangedFor
 | block # pvlBlock
 | 'goto' identifier ';' # pvlGoto
 | 'label' identifier ';' # pvlLabel
 | allowedForStatement ';' # pvlForStatement
 | 'communicate' access direction access ';' # pvlCommunicateStatement
 ;

direction
 : '<-'
 | '->'
 ;

access: subject '.' identifier;
subject
 : identifier
 | identifier '[' expr ']'
 | identifier '[' identifier ':' expr '..' expr ']';

elseBlock: 'else' statement;
barrierTags: ';' identifierList;
barrierBody: '{' contract '}' | contract block;

allowedForStatement
 : type declList # pvlLocal
 | expr # pvlEval
 | identifier ('++'|'--') # pvlIncDec
 | expr '=' expr # pvlAssign
 | expr ':' '=' expr # pvlSeqAssign
 ;

forStatementList
 : allowedForStatement
 | allowedForStatement ',' forStatementList
 ;

parRegion
 : 'parallel' '{' parRegion* '}' # pvlParallel
 | 'sequential' '{' parRegion* '}' # pvlSequential
 | 'par' identifier? parBlockIter? contract statement # pvlParBlock
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

nonArrayType
 : valType
 | ('int' | 'boolean' | 'void' | 'float32' | 'float64' | 'char')
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
declaredTypeArgs: '<' identifierList '>';

identifierList
 : identifier
 | identifier ',' identifierList
 ;

identifier : Identifier | LANG_ID_ESCAPE ;
grammar PVL;
import SpecParser;

@parser::members {
    private static int specLevel = 1;
}

VAL_INLINE: 'inline';
VAL_ASSERT: 'assert';

langExpr: expr;
langId: identifier;
langType: type;
langModifier: modifier;
langStatement: statement;
langDecl: EOF EOF;

program  : programDecl* block? EOF ;

programDecl : claz|kernel|block|field|methodDecl ;

claz : contract 'class' identifier '{' clazMember* '}' ;
clazMember : field | methodDecl | constructor;

kernel : 'kernel' identifier '{' kernelMember* '}' ;
kernelMember : kernelField | methodDecl ;

kernelField : ('global' | 'local') type identifierList ';' ;

field : type identifierList ';' ;

modifier : ( 'static' | 'thread_local' | 'inline' | 'pure' );

methodDecl : contract modifier* type identifier '(' args? ')' methodBody ;
methodBody : '=' expr ';' | constructorBody ;

constructor : contract identifier '(' args? ')' constructorBody ;
constructorBody : ';' | block ;

contract : valContractClause* ;

args
    : type identifier
    | type identifier ',' args
    ;

exprList
    : expr
    | expr ',' exprList
    ;
mapPairs
    : expr '->' expr
    | expr '->' expr ',' mapPairs
    ;

expr
 : identifier ':' expr
 | expr 'with' block
 | expr 'then' block
 | 'unfolding' expr 'in' expr
 | iteExpr
 ;

iteExpr
 : implicationExpr '?' implicationExpr ':' iteExpr
 | implicationExpr
 ;

implicationExpr
 : implicationExpr '==>' andOrExpr
 | implicationExpr '-*' andOrExpr
 | andOrExpr
 ;

andOrExpr
 : andOrExpr '&&' eqExpr
 | andOrExpr '||' eqExpr
 | andOrExpr '**' eqExpr
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
 | multExpr '\\' powExpr
 | powExpr
 ;

powExpr
 : powExpr '^^' unaryExpr
 | seqAddExpr
 ;

seqAddExpr
 : unaryExpr '::' seqAddExpr
 | seqAddExpr '++' unaryExpr
 | seqAddExpr '++' '(' unaryExpr ',' unaryExpr ')'
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
 | nonTarget
 | target
 ;

target
 : target '.' gen_id
 | target '[' expr ']'
 | nonTarget '.' gen_id
 | nonTarget '[' expr ']'
 | targetUnit
 ;

nonTarget
 : nonTarget '.' gen_id
 | nonTarget tuple
 | nonTarget '[' '..' expr ']'
 | nonTarget '[' expr ']'
 | nonTarget '[' expr '..' expr? ']'
 | nonTarget '[' expr '->' expr ']'
 | nonTarget '->' identifier tuple
 | nonTargetUnit
 ;

nonTargetUnit
 : 'this'
 | 'null'
 | 'true'
 | 'false'
 | 'current_thread'
 | '\\result'
 | collectionConstructors
 | 'map' '<' type ',' type '>' mapValues
 | 'tuple' '<' type ',' type '>' values
 | builtinMethod tuple
 | '\\owner' '(' expr ',' expr ',' expr ')'
 | 'id' '(' expr ')'
 | '|' expr '|'
 | '?' identifier
 | NUMBER
 | values
 | '(' expr ')'
 | identifier
 | valPrimary
 ;

collectionConstructors
 : container '<' type '>' values
 | '[' exprList ']'
 | '[t:' type ']'
 | '{' exprList '}'
 | '{t:' type '}'
 | 'b{' exprList '}'
 | 'b{t:' type '}'
 | 'set' '<' type '>' '{' expr '|' setCompSelectors ';' expr '}'
 ;

targetUnit
 : identifier
 ;

builtinMethod
 : ('Value' | 'HPerm' | 'Perm' | 'PointsTo' | 'Hist' | '\\old' | '?' | 'idle' | 'running' | 'head' | 'tail' | 'held' | 'Some')
 ;

values : '{' exprList? '}';

mapValues : '{' mapPairs? '}';

tuple : '(' exprList? ')';

block : '{' statement* '}' ;

setCompSelectors
    : type identifier
    | type identifier '<-' identifier
    | type identifier '<-' collectionConstructors
    | type identifier ',' setCompSelectors
    | type identifier '<-' identifier ',' setCompSelectors
    | type identifier '<-' collectionConstructors ',' setCompSelectors
    ;

statement
 : 'return' expr? ';'
 | 'lock' expr ';'
 | 'unlock' expr ';'
 | 'wait' expr ';'
 | 'notify' expr ';'
 | 'fork' expr ';'
 | 'join' expr ';'
 | 'action' tuple block
 | valStatement
 | 'if' '(' expr ')' statement elseBlock?
 | 'barrier' '(' identifier barrierTags? ')' barrierBody
 | contract 'par' parUnitList
 | 'vec' '(' iter ')' block
 | 'invariant' identifier '(' expr ')' block
 | 'atomic' '(' identifierList ')' block
 | invariantList 'while' '(' expr ')' statement
 | invariantList 'for' '(' forStatementList? ';' expr? ';' forStatementList? ')' statement
 | block
 | '{*' expr '*}'
 | 'goto' identifier ';'
 | 'label' identifier ';'
 | allowedForStatement ';'
 ;

elseBlock: 'else' statement;
barrierTags: ';' identifierList;
barrierBody: '{' contract '}' | contract block;
parUnitList: parUnit | parUnit 'and' parUnitList;

forStatementList
 : allowedForStatement
 | allowedForStatement ',' forStatementList
 ;

allowedForStatement
 : type declList
 | expr
 | identifier ('++'|'--')
 | target '=' expr
 ;

declList
 : identifier declInit?
 | identifier declInit? ',' declList
 ;

declInit
 : '=' expr
 ;

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
 : container '<' type '>'
 | 'option' '<' type '>'
 | ('map' | 'tuple') '<' type ',' type '>'
 | ('string' | 'process' | 'int' | 'boolean' | 'zfrac' | 'frac' | 'resource' | 'void')
 | classType
 ;

type
 : nonArrayType typeDims
 ;

typeDims
 : quantifiedDim*
 | anonDim*
 ;

newDims
 : quantifiedDim+
 ;

quantifiedDim
 : '[' expr ']'
 ;

anonDim
 : '[' ']'
 ;

gen_id : identifier | container ;

classType : identifier typeArgs?;

typeArgs : '<' exprList '>';


container : ('seq' | 'set' | 'bag');

identifierList
    : identifier
    | identifier ',' identifierList
    ;

identifier : Identifier | valReserved ;

Identifier  : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;
NUMBER : ('0'..'9')+;

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

// There's no such thing as embedding contracts in PVL
startSpec: EOF EOF;
endSpec: EOF EOF;
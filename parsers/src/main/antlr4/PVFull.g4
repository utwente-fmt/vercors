grammar PVFull;

import val;

@lexer::members {
    public static final int CH_COMMENT = 1;
    public static final int CH_LINEDIRECTION = 2;
}

expression : expr ;

program  : (claz|kernel|block|field|method_decl)* (block)? EOF ;

claz : contract 'class' identifier '{'( field | method_decl | constructor )* '}' ;

kernel : 'kernel' identifier '{' ( kernel_field | method_decl )* '}' ;

kernel_field : ('global' | 'local') type identifier ( ',' identifier )* ';' ;

field : type identifier ( ',' identifier )* ';' ;

modifiers : ( 'static' | 'thread_local' | 'inline' | 'pure' )*;

method_decl : contract modifiers type identifier '(' args ')' ( '=' expr ';' | ';' | block ) ;

constructor : contract identifier '(' args ')' ( block | ';' ) ;

contract : valContractClause* ;

args : type identifier ( ',' type identifier )* | ;


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
 | 'new' non_array_type new_dims
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
 | nonTarget '[' expr ('..' expr?)? ']'
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

arguments: (expression (',' expression)*);

collectionConstructors
 : CONTAINER '<' type '>' values
 | '[' arguments ']'
 | '[t:' type ']'
 | '{' arguments '}'
 | '{t:' type '}'
 | 'b{' arguments '}'
 | 'b{t:' type '}'
 ;

targetUnit
 : identifier
 ;

builtinMethod
 : 'Value' | 'HPerm' | 'Perm' | 'PointsTo' | 'Hist' | '\\old' | '?' | 'idle' | 'running' | 'head' | 'tail' | 'held' | 'Some'
 ;

mapValues : '{' ( | expr '->' expr (',' expr '->' expr)*) '}';

values : '{' ( | expr (',' expr)*) '}';

tuple : '(' ( | expr (',' expr)*) ')';

block : '{' statement* '}' ;

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
 | 'if' '(' expr ')' statement ( 'else' statement )?
 | 'barrier' '(' identifier ( ';' id_list )? ')' ( '{' contract '}' | contract block )
 | contract 'par' par_unit ( 'and' par_unit )*
 | 'vec' '(' iter ')' block
 | 'invariant' identifier '(' expr ')' block
 | 'atomic' '(' id_list ')' block
 | invariant 'while' '(' expr ')' statement
 | invariant 'for' '(' forStatementList? ';' expr? ';' forStatementList? ')' statement
 | block
 | '{*' expr '*}'
 | 'goto' identifier ';'
 | 'label' identifier ';'
 | allowedForStatement ';'
 ;

forStatementList
 : allowedForStatement (',' allowedForStatement)*
 ;

allowedForStatement
 : type identifier ('=' expr | (',' identifier)* )
 | expr
 | identifier ('++'|'--')
 | target '=' expr
 ;

par_unit
 : identifier? '(' iters ( ';' wait_list )? ')' contract block
 | contract block
 ;

wait_list : wait_for ( ',' wait_for )* ;

wait_for : identifier ( '(' id_arg_list ')' ) ? ;

id_arg_list : id_arg ( ',' id_arg )* ;

id_arg : identifier | '*' ;

id_list : ( identifier ( ',' identifier )* )? ;

with_then : ( 'with' block )? ('then' block)? ;

iters : ( iter ( ',' iter )* )? ;

iter : type identifier '=' expr '..' expr ;

decls  : ( decl ( ',' decl )* )? ;

decl : type identifier ( '=' expr )? ;

fence_list : ( 'local' | 'global' )* ;

invariant : ( 'loop_invariant' expr ';' )* ;

non_array_type
 : CONTAINER '<' type '>'
 | 'option' '<' type '>'
 | ('map' | 'tuple') '<' type ',' type '>'
 | ( 'string' | 'process' | 'int' | 'boolean' | 'zfrac' | 'frac' | 'resource' | 'void' | classType )
 ;

type
 : non_array_type type_dims
 ;

type_dims
 : ('[' expr ']')*
 | ('[' ']')*
 ;

new_dims
 : ('[' expr ']')+
 ;

gen_id : identifier | CONTAINER ;

classType : identifier typeArgs?;

typeArgs : '<' expr (',' expr)* '>';


CONTAINER : 'seq' | 'set' | 'bag' ;

identifier : Identifier | valReserved ;

Identifier  : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;
NUMBER : ('0'..'9')+;

COMMENT : '/*' .*? '*/' { setChannel(CH_COMMENT); } ;
LINE_COMMENT : '//' .*? '\n' { setChannel(CH_COMMENT); } ;

WS  :   (   ' '
        |   '\t'
        |   '\r'
        |   '\n'
        )+ -> skip ;

EmbeddedLatex
    : '#' ~[\r\n]* '#' -> skip
    ;

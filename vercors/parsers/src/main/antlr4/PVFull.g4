grammar PVFull;

import val;

@lexer::members {
    public static final int CH_COMMENT = 1;
    public static final int CH_LINEDIRECTION = 2;
}

expression : expr ;

program  : (claz|kernel|block|field|method_decl)* (block)? ;

claz : contract 'class' identifier '{'( field | method_decl | constructor )* '}' ;

kernel : 'kernel' identifier '{' ( kernel_field | method_decl )* '}' ;

kernel_field : ('global' | 'local') type identifier ( ',' identifier )* ';' ;

field : type identifier ( ',' identifier )* ';' ;

modifiers : ( 'static' | 'thread_local' | 'inline' | 'pure' )*;

method_decl : contract modifiers type identifier '(' args ')' ( '=' expr ';' | ';' | block ) ;

constructor : contract identifier '(' args ')' ( block | ';' ) ;

contract : valContractClause* ;

args : type identifier ( ',' type identifier )* | ;

atomExpression
 : lexpr
 | identifier ':' atomExpression
 | atomExpression 'with' block
 | atomExpression 'then' block
 | CONTAINER '<' type '>' values
 | ('!'|'-') atomExpression
 | atomExpression '^^' atomExpression
 | atomExpression ('*'|'/'|'%') atomExpression
 | atomExpression ( '+' | '-' ) atomExpression
 | atomExpression ( '<' | '<=' | '>=' | '>') atomExpression
 | atomExpression ( '==' | '!=' ) atomExpression
 | <assoc=right> atomExpression '::' atomExpression
 | atomExpression '++' atomExpression
 | atomExpression '[' atomExpression? '..' atomExpression? ']'
 //TODO fix that a[..] is not possible
 | atomExpression ('in' | '!in') atomExpression
 | '?' identifier
 | lexpr '->' identifier tuple
 | ( identifier | lexpr | 'Value' | 'HPerm' | 'Perm' | 'PointsTo' | 'Hist' | '\\old' | '?' ) tuple
 | '(' expr ')'
 | '\\owner' '(' expr ',' expr ',' expr ')'
 | 'id' '(' expr ')'
 | 'new' identifier tuple
 | 'new' non_array_type new_dims
 | 'null'
 | 'true'
 | 'false'
 | '\\result'
 | 'current_thread'
 | identifier
 | NUMBER
 | '|' expr '|'
 | values
 | 'unfolding' expr 'in' expr
 | simpleCollectionConstructors
 | valPrimary
 ;


arguments: ( | expression (',' expression)*);

simpleCollectionConstructors :
| '[' arguments ']'
| '[' type ']';

expr
 : atomExpression
 | expr ('&&'|'**') expr
 | expr '||' expr
 | expr ('==>'|'-*') expr
 | expr '?' expr ':' expr
 ;

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
 | lexpr '=' expr
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

lexpr : ('this' | '\\result' | identifier ) lexpr_access* ;

lexpr_access
 : '.' gen_id
 | '[' expr ']'
 ;

non_array_type
 : CONTAINER '<' type '>'
 | 'option' '<' type '>'
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

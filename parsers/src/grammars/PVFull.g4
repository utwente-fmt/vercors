grammar PVFull;

import val;

@header {
package vct.antlr4.generated;
}

@lexer::members{
//  public final static int COMMENT=ML_COMMENT;
  public final static int LINEDIRECTION=Integer.MAX_VALUE;
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
 | atomExpression 'in' atomExpression
 | '?' identifier
 | lexpr '->' identifier tuple
 | ( identifier | lexpr | 'Value' | 'HPerm' | 'Perm' | 'PointsTo' | 'Hist' | '\\old' | '?' ) tuple
 | '(' expr ')'
 | '\\owner' '(' expr ',' expr ',' expr ')'
 | 'id' '(' expr ')'
 | 'new' identifier tuple
 | 'new' type '[' expr ']'
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
 | valPrimary
 ;

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

simple_statement
 : 'return' expr?
 | 'lock' expr
 | 'unlock' expr
 | 'wait' expr
 | 'notify' expr
 | 'fork' expr
 | 'join' expr
 | type identifier ('=' expr | (',' identifier)* )
 | expr
 | identifier ('++'|'--')
 | lexpr '=' expr
 | 'goto' identifier
 | 'label' identifier
 ;

statement
 : simple_statement ';'
 | 'action' tuple block
 | valStatement 
 | 'if' '(' expr ')' block ( 'else' block )?
 | 'barrier' '(' identifier ( ';' id_list )? ')' ( '{' contract '}' | contract block )
 | contract 'par' par_unit ( 'and' par_unit )* 
 | 'vec' '(' iter ')' block 
 | 'invariant' identifier '(' expr ')' block 
 | 'atomic' '(' id_list ')' block 
 | invariant 'while' '(' expr ')' block
 | block
 | '{*' expr '*}'
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

lexpr : ('this' | '\\result' | identifier ) ('.' gen_id | '[' expr ']' )* ; 

type
 : CONTAINER '<' type '>'
 | 'option' '<' type '>'
 | ( 'string' | 'process' | 'int' | 'boolean' | 'zfrac' | 'frac' | 'resource' | 'void' | classType ) ('[' expr? ']')*
 ;

gen_id : identifier | CONTAINER ; 

classType : identifier typeArgs?;

typeArgs : '<' expr (',' expr)* '>';


CONTAINER : 'seq' | 'set' | 'bag' ;

identifier : Identifier | valReserved ;

Identifier  : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;
NUMBER : ('0'..'9')+;

COMMENT : '/*' .*? '*/' -> channel(COMMENT) ;
LINE_COMMENT : '//' .*? '\n' -> channel(COMMENT) ;

WS  :   (   ' '
        |   '\t'
        |   '\r'
        |   '\n'
        )+ -> skip ;

EmbeddedLatex
    : '#' ~[\r\n]* '#' -> skip
    ;

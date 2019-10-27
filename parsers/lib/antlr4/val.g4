grammar val;

/**
 imported grammar rules
   expression
   identifier
   type
   block
 exported grammar rules
   valContractClause - contract clause
   valStatement      - proof guiding statements
   valReserved       - reserved identifiers
 */
 

valContractClause
 : 'modifies' expression (',' expression)* ';'
 | 'accessible' expression (',' expression)* ';'
 | 'requires' expression ';'
 | 'ensures' expression ';'
 | 'given' type identifier ';'
 | 'yields' type identifier ';'
 | 'context_everywhere' expression ';'
 | 'context' expression ';'
 ;

valStatement
 : 'loop_invariant' expression ';'
 | 'create' block               // create a magic wand
 | 'qed' expression ';'
 | 'apply' expression ';'
 | 'use' expression ';'
 | 'create' expression ';'             // create empty history
 | 'create' expression ',' expression ';'   // create given future
 | 'destroy' expression ',' expression ';'  // destroy given
 | 'destroy' expression ';'           // destroy empty future
 | 'split' expression ',' expression ',' expression ',' expression ',' expression ';'
 | 'merge' expression ',' expression ',' expression ',' expression ',' expression ';'
 | 'choose' expression ',' expression ',' expression ',' expression ';'
 | 'fold' expression ';'
 | 'unfold' expression ';'
 | 'open' expression ';'
 | 'close' expression ';'
 | 'assert' expression ';' 
 | 'assume' expression ';'
 | 'inhale' expression ';' 
 | 'exhale' expression ';'
 | 'label' identifier ';' 
 | 'refute' expression ';' 
 | 'witness' expression ';'
 | 'ghost' block
 | 'send' expression 'to' Identifier ',' expression ';'
 | 'recv' expression 'from' Identifier ',' expression ';'
 | 'transfer' expression ';' 
 | 'csl_subject' expression ';'
 | 'spec_ignore' '}'
 | 'spec_ignore' '{'
 | 'action' expression ',' expression ',' expression ',' expression ( ',' expression ',' expression )* ';'
 | 'atomic' '(' ( expression (',' expression)* )? ')' block
 ;

valPrimary
    : type '{' ( expression (',' expression)* )? '}'
    | '[' expression ']' expression
    | '|' expression '|'
    | '\\unfolding' expression '\\in' expression
    | '(' expression '!' Identifier ')'
    | '(' expression '\\memberof' expression ')'
    | '['  expression '..' expression ')'
    | '*'
    | '\\current_thread'
    | '(' '\\forall*' type identifier '=' expression '..' expression ';' expression ')' 
    | '(' '\\forall*' type identifier ';' expression ';' expression ')'
    | '(' '\\forall' type identifier ';' expression ';' expression ')'
    | '(' '\\forall' type identifier '=' expression '..' expression ';' expression ')' 
    | '(' '\\exists' type identifier ';' expression ';' expression ')'
    | '(' '\\let' type identifier '=' expression ';' expression ')'
    | '(' '\\sum' type identifier ';' expression ';' expression ')'
    | '\\length' '(' expression ')'
    | '\\old' '(' expression ')'
    | '\\id' '(' expression ')'
    | '\\typeof' '(' expression ')'
    | '\\matrix' '(' expression ',' expression ',' expression ')'
    | '\\array'  '(' expression ',' expression ')'
    | '\\pointer' '(' expression ',' expression ',' expression ')'
    | '\\pointer_index' '(' expression ',' expression ',' expression ')'
    | '\\values' '(' expression ',' expression ',' expression ')'
    | '\\sum' '(' expression ',' expression ')'
    | '\\vcmp' '(' expression ',' expression ')'
    | '\\vrep' '(' expression ')'
    | '\\msum' '(' expression ',' expression ')'
    | '\\mcmp' '(' expression ',' expression ')'
    | '\\mrep' '(' expression ')'
    | 'Reducible' '(' expression ',' ('+' | Identifier ) ')'
    ;

valReserved
 : 'create' | 'action' | 'destroy' | 'send' | 'recv' | 'use' | 'open' | 'close'
 | 'atomic'  | 'from' | 'merge' | 'split' | 'process' | 'apply' | 'label'
 | '\\result' | '\\current_thread'
 ;


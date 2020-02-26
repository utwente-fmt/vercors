parser grammar SpecParser;

/**
 imported grammar rules
   expression
   identifier
   type
   statement
   startSpec - the rule entering the lexer into specification mode
   endSpec - the rule exiting the lexer from specification mode
 exported grammar rules for PVL
   valContractClause        - contract clause
   valStatement             - proof guiding statements
   valWithThen              - with/then statement to use given/yields ghost arguments
   valReserved              - reserved identifiers
 exported grammar rules for other languages
   valEmbedContract         - sequence of contract clauses embedded in sequence of comments
   valEmbedContractBlock    - sequence of contract clauses embedded in one comment
   valEmbedStatementBlock   - sequence of valStatements embedded in a comment
   valEmbedWithThenBlock    - with and/or then in one comment
   valEmbedWithThen         - with and/or then in a sequence of comments
 */

valExpressionList
    : expression
    | expression ',' valExpressionList
    ;

valLabelList
    : identifier
    | identifier ',' valLabelList
    ;

valContractClause
 : 'modifies' valExpressionList ';'
 | 'accessible' valExpressionList ';'
 | 'requires' expression ';'
 | 'ensures' expression ';'
 | 'given' type identifier ';'
 | 'yields' type identifier ';'
 | 'context_everywhere' expression ';'
 | 'context' expression ';'
 | 'loop_invariant' expression ';'
 ;

valBlock
 : '{' valStatement* '}'
 ;

valStatement
 : 'create' valBlock               // create a magic wand
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
 | 'ghost' {ghostLevel++;} statement {ghostLevel--;}
 | 'send' expression 'to' Identifier ',' expression ';'
 | 'recv' expression 'from' Identifier ',' expression ';'
 | 'transfer' expression ';' 
 | 'csl_subject' expression ';'
 | 'spec_ignore' '}'
 | 'spec_ignore' '{'
 | 'action' expression ',' expression ',' expression ',' expression ( ',' expression ',' expression )* ';'
 | 'atomic' '(' valLabelList? ')' statement
 ;

valWithThenMapping
 : identifier '=' expression ';'
 ;

valWithThen
 : 'with' '{' valWithThenMapping* '}'
 | 'then' '{' valWithThenMapping* '}'
 ;

valPrimary
    : type '{' valExpressionList? '}'
    | '[' expression ']' expression
    | '|' expression '|'
    | '\\unfolding' expression '\\in' expression
    | '(' expression '!' Identifier ')'
    | '(' expression '\\memberof' expression ')'
    | '['  expression '..' expression ')'
    | '*'
    | '\\current_thread'
    | '(' ('\\forall*'|'\\forall'|'\\exists')
        type identifier '=' expression '..' expression ';' expression ')'
    | '(' ('\\forall*'|'\\forall'|'\\exists')
        type identifier ';' expression ';' expression ')'
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
 : ('create' | 'action' | 'destroy' | 'send' | 'recv' | 'use' | 'open' | 'close'
        | 'atomic'  | 'from' | 'merge' | 'split' | 'process' | 'apply' | 'label')
 | '\\result'
 | '\\current_thread'
 | 'none' // No permission
 | 'write' // Full permission
 | 'read' // Any read permission
 | 'None' // The empty value of the option type
 | 'empty' // The empty process in the context of Models
 ;

valEmbedContract: valEmbedContractBlock+;

valEmbedContractBlock
 : startSpec valContractClause* endSpec
 ;

valEmbedStatementBlock
 : startSpec valStatement* endSpec
 ;

valEmbedWithThenBlock
 : startSpec valWithThen* endSpec
 ;

valEmbedWithThen
 : valEmbedWithThenBlock+
 ;
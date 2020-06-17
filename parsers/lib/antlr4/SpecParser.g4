parser grammar SpecParser;

/**
 imported grammar rules
   langExpr
   langId
   langType
   langModifier
   langStatement
   startSpec - the rule entering the lexer into specification mode
   endSpec - the rule exiting the lexer from specification mode
 exported grammar rules for PVL
   valContractClause        - contract clause
   valStatement             - proof guiding statement
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
    : langExpr
    | langExpr ',' valExpressionList
    ;

valContractClause
 : 'modifies' valExpressionList ';'
 | 'accessible' valExpressionList ';'
 | 'requires' langExpr ';'
 | 'ensures' langExpr ';'
 | 'given' langType langId ';'
 | 'yields' langType langId ';'
 | 'context_everywhere' langExpr ';'
 | 'context' langExpr ';'
 | 'loop_invariant' langExpr ';'
 | 'signals' '(' langType langId ')' langExpr ';'
 ;

valBlock
 : '{' valStatement* '}'
 ;

valStatement
 : 'create' valBlock               // create a magic wand
 | 'qed' langExpr ';'
 | 'apply' langExpr ';'
 | 'use' langExpr ';'
 | 'create' langExpr ';'             // create empty history
 | 'create' langExpr ',' langExpr ';'   // create given future
 | 'destroy' langExpr ',' langExpr ';'  // destroy given
 | 'destroy' langExpr ';'           // destroy empty future
 | 'split' langExpr ',' langExpr ',' langExpr ',' langExpr ',' langExpr ';'
 | 'merge' langExpr ',' langExpr ',' langExpr ',' langExpr ',' langExpr ';'
 | 'choose' langExpr ',' langExpr ',' langExpr ',' langExpr ';'
 | 'fold' langExpr ';'
 | 'unfold' langExpr ';'
 | 'open' langExpr ';'
 | 'close' langExpr ';'
 | 'assert' langExpr ';'
 | 'assume' langExpr ';'
 | 'inhale' langExpr ';'
 | 'exhale' langExpr ';'
 | 'label' langId ';'
 | 'refute' langExpr ';'
 | 'witness' langExpr ';'
 | 'ghost' langStatement
 | 'send' langExpr 'to' langId ',' langExpr ';'
 | 'recv' langExpr 'from' langId ',' langExpr ';'
 | 'transfer' langExpr ';'
 | 'csl_subject' langExpr ';'
 | 'spec_ignore' '}'
 | 'spec_ignore' '{'
 | 'action' langExpr ',' langExpr ',' langExpr ',' langExpr valActionMap* ';'
 | 'atomic' '(' valExpressionList? ')' langStatement
 ;

valActionMap: ',' langExpr ',' langExpr;

valWithThen
 : 'with' langStatement
 | 'then' langStatement
 ;

valImpOp: '-*' | '==>';
valAndOp: '**';
valMulOp: '\\';

valPrimary
    : langType '{' valExpressionList? '}'
    | '[' langExpr ']' langExpr
    | '|' langExpr '|'
    | '\\unfolding' langExpr '\\in' langExpr
    | '(' langExpr '!' langId ')'
    | '(' langExpr '\\memberof' langExpr ')'
    | '{'  langExpr '..' langExpr '}'
    | '*'
    | '\\current_thread'
    | '(' ('\\forall*'|'\\forall'|'\\exists')
        langType langId '=' langExpr '..' langExpr ';' langExpr ')'
    | '(' ('\\forall*'|'\\forall'|'\\exists')
        langType langId ';' langExpr ';' langExpr ')'
    | '(' '\\let' langType langId '=' langExpr ';' langExpr ')'
    | '(' '\\sum' langType langId ';' langExpr ';' langExpr ')'
    | '\\length' '(' langExpr ')'
    | '\\old' '(' langExpr ')'
    | '\\id' '(' langExpr ')'
    | '\\typeof' '(' langExpr ')'
    | '\\matrix' '(' langExpr ',' langExpr ',' langExpr ')'
    | '\\array'  '(' langExpr ',' langExpr ')'
    | '\\pointer' '(' langExpr ',' langExpr ',' langExpr ')'
    | '\\pointer_index' '(' langExpr ',' langExpr ',' langExpr ')'
    | '\\values' '(' langExpr ',' langExpr ',' langExpr ')'
    | '\\sum' '(' langExpr ',' langExpr ')'
    | '\\vcmp' '(' langExpr ',' langExpr ')'
    | '\\vrep' '(' langExpr ')'
    | '\\msum' '(' langExpr ',' langExpr ')'
    | '\\mcmp' '(' langExpr ',' langExpr ')'
    | '\\mrep' '(' langExpr ')'
    | 'Reducible' '(' langExpr ',' valReducibleOperator ')'
    | langId ':' langExpr
    ;

valReducibleOperator
    : '+'
    | langId
    ;

valReserved
 : (VAL_INLINE | VAL_ASSERT | VAL_RESOURCE | VAL_PROCESS | VAL_FRAC | VAL_ZFRAC | VAL_BOOL | VAL_RATIONAL | VAL_SEQ
    | VAL_PURE | VAL_THREAD_LOCAL | VAL_WITH | VAL_THEN | VAL_GIVEN | VAL_YIELDS | VAL_AXIOM | VAL_MODIFIES
    | VAL_ACCESSIBLE | VAL_REQUIRES | VAL_ENSURES | VAL_CONTEXT_EVERYWHERE | VAL_CONTEXT | VAL_LOOP_INVARIANT
    | VAL_CREATE | VAL_QED | VAL_APPLY | VAL_USE | VAL_DESTROY | VAL_SPLIT | VAL_MERGE | VAL_CHOOSE | VAL_FOLD
    | VAL_UNFOLD | VAL_OPEN | VAL_CLOSE | VAL_ASSUME | VAL_INHALE | VAL_EXHALE | VAL_LABEL | VAL_REFUTE | VAL_WITNESS
    | VAL_GHOST | VAL_SEND | VAL_WORD_TO | VAL_RECV | VAL_FROM | VAL_TRANSFER | VAL_CSL_SUBJECT | VAL_SPEC_IGNORE
    | VAL_ACTION | VAL_ATOMIC | VAL_REDUCIBLE)
 | '\\result'
 | '\\current_thread'
 | 'none' // No permission
 | 'write' // Full permission
 | 'read' // Any read permission
 | 'None' // The empty value of the option langType
 | 'empty' // The empty process in the context of Models
 ;

valType
 : ('resource' | 'process' | 'frac' | 'zfrac' | 'rational' | 'bool')
 | 'seq' '<' langType '>'
 ;

valDeclaration
 : valContractClause* valModifier* langType langId '(' valArgList? ')' valPredicateDef
 | 'axiom' langId '{' langExpr '==' langExpr '}'
 | valContractClause* 'ghost' langDecl
 ;

valPredicateDef
 : ';'
 | '=' langExpr ';'
 ;

valModifier
 :('pure'
 | 'inline'
 | 'thread_local'
)| langModifier
 ;

valArgList
 : valArg
 | valArg ',' valArgList
 ;

valArg
 : langType langId
 ;

valEmbedContract: valEmbedContractBlock+;

valEmbedContractBlock
 : startSpec valContractClause* endSpec
 | {specLevel>0}? valContractClause+
 ;

valEmbedStatementBlock
 : startSpec valStatement* endSpec
 | {specLevel>0}? valStatement+
 ;

valEmbedWithThenBlock
 : startSpec valWithThen* endSpec
 | {specLevel>0}? valWithThen+
 ;

valEmbedWithThen
 : valEmbedWithThenBlock+
 ;

valEmbedDeclarationBlock
 : startSpec valDeclaration* endSpec
 ;

valEmbedModifiers
 : startSpec valModifier* endSpec
 | {specLevel>0}? valModifier+
 ;
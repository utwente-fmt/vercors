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

valIdList
 : langId
 | langId ',' valIdList
 ;

valContractClause
 : 'modifies' valIdList ';'
 | 'accessible' valIdList ';'
 | 'requires' langExpr ';'
 | 'ensures' langExpr ';'
 | 'given' langType langId ';'
 | 'yields' langType langId ';'
 | 'context_everywhere' langExpr ';'
 | 'context' langExpr ';'
 | 'loop_invariant' langExpr ';'
 | 'kernel_invariant' langExpr ';'
 | 'signals' '(' langType langId ')' langExpr ';'
 ;

valBlock
 : '{' valStatement* '}'
 ;

valStatement
 : 'create' valBlock # valCreateWand
 | 'qed' langExpr ';' # valQedWand
 | 'apply' langExpr ';' # valApplyWand
 | 'use' langExpr ';' # valUseWand
 | 'create' langExpr ';' # valCreateModel
 | 'create' langExpr ',' langExpr ';' # valCreateModel2
 | 'destroy' langExpr ',' langExpr ';' # valDestroyModel
 | 'destroy' langExpr ';' # valDestroyModel2
 | 'split' langExpr ',' langExpr ',' langExpr ',' langExpr ',' langExpr ';' # valSplitModel
 | 'merge' langExpr ',' langExpr ',' langExpr ',' langExpr ',' langExpr ';' # valMergeModel
 | 'choose' langExpr ',' langExpr ',' langExpr ',' langExpr ';' # valChooseModel
 | 'fold' langExpr ';' # valFold
 | 'unfold' langExpr ';' # valUnfold
 | 'open' langExpr ';' # valOpen
 | 'close' langExpr ';' # valClose
 | 'assert' langExpr ';' # valAssert
 | 'assume' langExpr ';' # valAssume
 | 'inhale' langExpr ';' # valInhale
 | 'exhale' langExpr ';' # valExhale
 | 'label' langId ';' # valLabel
 | 'refute' langExpr ';' # valRefute
 | 'witness' langExpr ';' # valWitness
 | 'ghost' langStatement # valGhost
 | 'send' langExpr 'to' langId ',' langExpr ';' # valSend
 | 'recv' langExpr 'from' langId ',' langExpr ';' # valRecv
 | 'transfer' langExpr ';' # valTransfer
 | 'csl_subject' langExpr ';' # valCslSubject
 | 'spec_ignore' '}' # valSpecIgnoreStart
 | 'spec_ignore' '{' # valSpecIgnoreEnd
 | 'action' langExpr ',' langExpr ',' langExpr ',' langExpr valActionMap* ';' # valActionModel
 | 'atomic' '(' langId ')' langStatement # valAtomic
 ;

valActionMap: ',' langExpr ',' langExpr;

valWithThen
 : 'with' langStatement # valWith
 | 'then' langStatement # valThen
 ;

valImpOp: '-*' | '==>';
valAndOp: '**';
valInOp: '\\in';
valMulOp: '\\';
valPrependOp : '::';
valAppendOp : '++'; // postfix issues? maybe disable in spec - no side effects?
valPostfix
 : '[' '..' langExpr ']'
 | '[' langExpr '..' langExpr? ']'
 | '[' langExpr '->' langExpr ']' // C?
 ;

valPrimarySeq
 : '|' langExpr '|' # valCardinality
 | 'isEmpty' '(' langExpr ')' # valIsEmpty
 | 'head' '(' langExpr ')' # valHead
 | 'tail' '(' langExpr ')' # valTail
 | ('remove'|'removeAt') '(' langExpr ',' langExpr ')' # valRemove
 | '\\values' '(' langExpr ',' langExpr ',' langExpr ')' # valArrayValues
 ;

valPrimaryMap
 : 'buildMap' '(' langExpr ',' langExpr ',' langExpr ')' # valBuildMap
 | 'cardMap' '(' langExpr ')' # valCardMap
 | 'valuesMap' '(' langExpr ')' # valValuesMap
 | 'removeFromMap' '(' langExpr ',' langExpr ')' # valRemoveMap
 | 'itemsMap' '(' langExpr ')' # valItemsMap
 | 'keysMap' '(' langExpr ')' # valKeysMap
 | 'getFromMap' '(' langExpr ',' langExpr ')' # valGetMap
 | 'disjointMap' '(' langExpr ',' langExpr ')' # valDisjointMap
 | 'equalsMap' '(' langExpr ',' langExpr ')' # valEqualsMap
 ;

valPrimaryOption
 : 'getOption' '(' langExpr ')' # valGetOption
 | 'getOrElseOption' '(' langExpr ',' langExpr ')' # valGetOrElseOption
 | 'Some' '(' langExpr ')' # valSome
 ;

valPrimaryTuple
 : 'getFst' '(' langExpr ')' # valFst
 | 'getSnd' '(' langExpr ')' # valSnd
 ;

valSetCompSelectors
 : langType langId
 | langType langId '<-' langId
 | langType langId '<-' valPrimaryCollectionConstructor
 | langType langId ',' valSetCompSelectors
 | langType langId '<-' langId ',' valSetCompSelectors
 | langType langId '<-' valPrimaryCollectionConstructor ',' valSetCompSelectors
 ;

valMapPairs
 : langExpr '->' langExpr
 | langExpr '->' langExpr ',' valMapPairs
 ;

valPrimaryCollectionConstructor
 : 'seq' '<' langType '>' '{' valExpressionList? '}' # valTypedLiteralSeq
 | 'set' '<' langType '>' '{' valExpressionList? '}' # valTypedLiteralSet
 | 'set' '<' langType '>' '{' langExpr '|' valSetCompSelectors ';' langExpr '}' # valSetComprehension
 | 'bag' '<' langType '>' '{' valExpressionList? '}' # valTypedLiteralBag
 | 'map' '<' langType ',' langType '>' '{' valMapPairs? '}' # valTypedLiteralMap
 | 'tuple' '<' langType ',' langType '>' '{' langExpr ',' langExpr '}' # valTypedTuple
 | '[' valExpressionList ']' # valLiteralSeq
 | '{' valExpressionList '}' # valLiteralSet
 | 'b{' valExpressionList '}' # valLiteralBag
 | '[t:' langType ']' # valEmptySeq
 | '{t:' langType '}' # valEmptySet
 | 'b{t:' langType '}' # valEmptyBag
 | '{' langExpr '..' langExpr '}' # valRange
 ;

valPrimaryPermission
 : 'perm' '(' langExpr ')' # valCurPerm
 | 'Perm' '(' langExpr ',' langExpr ')' # valPerm
 | 'Value' '(' langExpr ')' # valValue
 | 'PointsTo' '(' langExpr ',' langExpr ',' langExpr ')' #valPointsTo
 | 'HPerm' '(' langExpr ',' langExpr ')' # valHPerm
 | 'APerm' '(' langExpr ',' langExpr ')' # valAPerm
 | 'ArrayPerm' '(' langExpr ',' langExpr ',' langExpr ',' langExpr ',' langExpr ')' # valArrayPerm
 | '\\matrix' '(' langExpr ',' langExpr ',' langExpr ')' # valMatrix
 | '\\array'  '(' langExpr ',' langExpr ')' # valArray
 | '\\pointer' '(' langExpr ',' langExpr ',' langExpr ')' # valPointer
 | '\\pointer_index' '(' langExpr ',' langExpr ',' langExpr ')' # valPointerIndex
 ;

valPrimaryBinder
 : '(' ('\\forall*'|'\\forall'|'\\exists')
        langType langId '=' langExpr '..' langExpr ';' langExpr ')' # valRangeQuantifier
 | '(' ('\\forall*'|'\\forall'|'\\exists')
        langType langId ';' langExpr ';' langExpr ')' # valQuantifier
 | '(' '\\let' langType langId '=' langExpr ';' langExpr ')' # valLet
 ;

valPrimaryVector
 : '(' '\\sum' langType langId ';' langExpr ';' langExpr ')' # valSum
 | '\\sum' '(' langExpr ',' langExpr ')' # valVectorSum
 | '\\vcmp' '(' langExpr ',' langExpr ')' # valVectorCmp
 | '\\vrep' '(' langExpr ')' # valVectorRep
 | '\\msum' '(' langExpr ',' langExpr ')' # valMatrixSum
 | '\\mcmp' '(' langExpr ',' langExpr ')' # valMatrixCmp
 | '\\mrep' '(' langExpr ')' # valMatrixRep
 ;

valPrimaryModel
 : 'AbstractState' '(' langExpr ',' langExpr ')' # valAbstractState
 | 'Future' '(' langExpr  ',' langExpr ',' langExpr ')' # valFuture
 | 'Hist' '(' langExpr ',' langExpr ',' langExpr ')' # valHist
 ;

valPrimaryReducible
 : 'Reducible' '(' langExpr ',' valReducibleOperator ')' # valReducible
 | 'Contribution' '(' langExpr ',' langExpr ')' # valContribution
 ;

valReducibleOperator
 : '+'
 | langId
 ;

valPrimaryThread
 : 'idle' '(' langExpr ')' # valIdle
 | 'running' '(' langExpr ')' # valRunning
 ;

valPrimary
 : valPrimarySeq
 | valPrimaryMap
 | valPrimaryOption
 | valPrimaryTuple
 | valPrimaryCollectionConstructor
 | valPrimaryPermission
 | valPrimaryBinder
 | valPrimaryVector
 | valPrimaryModel
 | valPrimaryReducible
 | valPrimaryThread
 | '*' # valAny
 | '(' langExpr '!' langId ')' # valIndependent
 | '[' langExpr ']' langExpr # valScale
 | '{:' langExpr ':}' # valInlinePattern
 | '\\unfolding' langExpr '\\in' langExpr # valUnfolding
 | '\\old' '(' langExpr ')' # valOld
 | '\\typeof' '(' langExpr ')' # valTypeof
 | 'held' '(' langExpr ')' # valHeld
 ;

valReserved
 : (VAL_INLINE | VAL_ASSERT | VAL_RESOURCE | VAL_PROCESS | VAL_FRAC | VAL_ZFRAC | VAL_BOOL | VAL_RATIONAL | VAL_SEQ
    | VAL_PURE | VAL_THREAD_LOCAL | VAL_WITH | VAL_THEN | VAL_GIVEN | VAL_YIELDS | VAL_AXIOM | VAL_MODIFIES
    | VAL_ACCESSIBLE | VAL_REQUIRES | VAL_ENSURES | VAL_CONTEXT_EVERYWHERE | VAL_CONTEXT | VAL_LOOP_INVARIANT
    | VAL_CREATE | VAL_QED | VAL_APPLY | VAL_USE | VAL_DESTROY | VAL_SPLIT | VAL_MERGE | VAL_CHOOSE | VAL_FOLD
    | VAL_UNFOLD | VAL_OPEN | VAL_CLOSE | VAL_ASSUME | VAL_INHALE | VAL_EXHALE | VAL_LABEL | VAL_REFUTE | VAL_WITNESS
    | VAL_GHOST | VAL_SEND | VAL_WORD_TO | VAL_RECV | VAL_FROM | VAL_TRANSFER | VAL_CSL_SUBJECT | VAL_SPEC_IGNORE
    | VAL_ACTION | VAL_ATOMIC | VAL_REDUCIBLE | VAL_SIGNALS | VAL_SET | VAL_BAG | VAL_LOC | VAL_ABSTRACT_STATE
    | VAL_ADDS_TO | VAL_APERM | VAL_ARRAYPERM | VAL_BUILD_MAP | VAL_CARD_MAP | VAL_CONTRIBUTION
    | VAL_DISJOINT_MAP | VAL_EQUALS_MAP | VAL_FUTURE | VAL_GET_FROM_MAP | VAL_GET_FST | VAL_GET_OPTION | VAL_GET_SND
    | VAL_HEAD | VAL_HELD | VAL_HIST | VAL_HPERM | VAL_IDLE | VAL_IS_EMPTY | VAL_ITEMS_MAP | VAL_KEYS_MAP | VAL_PERM_VAL
    | VAL_PERM | VAL_POINTS_TO | VAL_REMOVE | VAL_REMOVE_AT | VAL_REMOVE_FROM_MAP | VAL_RUNNING | VAL_SOME | VAL_TAIL
    | VAL_VALUE | VAL_VALUES_MAP | VAL_POINTER | VAL_KERNEL_INVARIANT | VAL_GETOPTELSE | VAL_MAP | VAL_OPTION | VAL_TUPLE)
 | LANG_ID_ESCAPE # valIdEscape
 | '\\result' # valResult
 | '\\current_thread' # valCurrentThread
 | 'none' # valNonePerm // No permission
 | 'write' # valWrite // Full permission
 | 'read' # valRead // Any read permission
 | 'None' # valNoneOption // The empty value of the option langType
 | 'empty' # valEmpty // The empty process in the context of Models
 | '\\ltid' # valLtid
 | '\\gtid' # valGtid
 | 'true' # valTrue
 | 'false' # valFalse
 ;

valType
 : ('resource' | 'process' | 'frac' | 'zfrac' | 'rational' | 'bool') # valPrimaryType
 | 'seq' '<' langType '>' # valSeqType
 | 'set' '<' langType '>' # valSetType
 | 'bag' '<' langType '>' # valBagType
 | 'option' '<' langType '>' # valOptionType
 | 'map' '<' langType ',' langType '>' # valMapType
 | 'tuple' '<' langType ',' langType '>' # valTupleType
 | 'pointer' '<' langType '>' # valPointerType
 ;

valGlobalDeclaration
 : 'axiom' langId '{' langExpr '}' # valAxiom
 | valModifier* 'resource' langId '(' valArgList? ')' valDef # valPredicate
 | valContractClause* valModifier* 'pure' langType langId '(' valArgList? ')' valDef # valFunction
 | 'temporaryUnusedKeywordForModel' langId '{' valModelDeclaration* '}' # valModel
 | 'ghost' langGlobalDecl # valGhostDecl
 ;

valClassDeclaration
 : valModifier* 'resource' langId '(' valArgList? ')' valDef # valInstancePredicate
 | valContractClause* valModifier* 'pure' langType langId '(' valArgList? ')' valDef # valInstanceFunction
 | 'ghost' langClassDecl # valInstanceGhostDecl
 ;

valModelDeclaration
 : langType langId ';' # valModelField
 | valContractClause* 'process' langId '(' valArgList? ')' '=' langExpr ';' # valModelProcess
 | valContractClause* 'action' langId '(' valArgList? ')' ';' # valModelAction
 ;

valDef
 : ';' # valAbstractBody
 | '=' langExpr ';' # valBody
 ;

valModifier
 : ('pure' | 'inline' | 'thread_local')
 | langStatic # valStatic
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

valEmbedGlobalDeclarationBlock
 : startSpec valGlobalDeclaration* endSpec
 | {specLevel>0}? valGlobalDeclaration+
 ;

valEmbedClassDeclarationBlock
 : startSpec valClassDeclaration* endSpec
 | {specLevel>0}? valClassDeclaration+
 ;

valEmbedModifier
 : startSpec valModifier endSpec
 | {specLevel>0}? valModifier
 ;
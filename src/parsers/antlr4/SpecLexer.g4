lexer grammar SpecLexer;

/*
These tokens overlap one of the frontends, which leads to problems in ANTLR. They must be reproduced in the lexer of a
frontend, if the lexer of the frontend does not already define the token.

COMMA: ',';
SEMI: ';';
BLOCK_OPEN: '{';
BLOCK_CLOSE: '}';
PAREN_OPEN: '(';
PAREN_CLOSE: ')';
BRACK_OPEN: '[';
BRACK_CLOSE: ']';
ANGLE_OPEN: '<';
ANGLE_CLOSE: '>';
EQ: '=';
EQUALS: '==';
EXCL: '!';
STAR: '*';
PIPE: '|';
PLUS: '+';
COLON: ':';
CONS: '::';
VAL_INLINE: {"keyword.spec";} 'inline';
VAL_ASSERT: {"keyword.spec";} 'assert';
VAL_TRUE: {"keyword.spec";} 'true';
VAL_FALSE: {"keyword.spec";} 'false';
VAL_PACKAGE: 'package';
*/

// Must be able to contain identifiers from any frontend, so it's fine to over-approximate valid identifiers a bit.
LANG_ID_ESCAPE: '`' ~[`]+ '`';

VAL_RESOURCE: {"keyword.spec";} 'resource';
VAL_PROCESS: {"keyword.spec";} 'process';
VAL_FRAC: {"keyword.spec";} 'frac';
VAL_ZFRAC: {"keyword.spec";} 'zfrac';
VAL_BOOL: {"keyword.spec";} 'bool';
VAL_REF: {"keyword.spec";} 'ref';
VAL_RATIONAL: {"keyword.spec";} 'rational';
VAL_SEQ: {"type.spec";} 'seq';
VAL_SET: {"type.spec";} 'set';
VAL_BAG: {"type.spec";} 'bag';
VAL_POINTER: {"type.spec";} 'pointer';
VAL_MAP: {"type.spec";} 'map';
VAL_OPTION: {"type.spec";} 'option';
VAL_EITHER: {"type.spec";} 'either';
VAL_TUPLE: {"type.spec";} 'tuple';
VAL_TYPE: {"type.spec";} 'type';
VAL_ANY: {"type.spec";} 'any';
VAL_NOTHING: {"type.spec";} 'nothing';
VAL_STRING: {"type.spec";} 'string';

VAL_PURE: {"type.spec";} 'pure';
VAL_THREAD_LOCAL: {"type.spec";} 'thread_local';
VAL_BIP_ANNOTATION: {"keyword.spec";} 'bip_annotation';

VAL_WITH: {"keyword.control.spec";} 'with';
VAL_THEN: {"keyword.control.spec";} 'then';
VAL_GIVEN: {"keyword.control.spec";} 'given';
VAL_YIELDS: {"keyword.control.spec";} 'yields';

VAL_AXIOM: {"type.spec";} 'axiom';
VAL_MODEL: {"type.spec";} 'model';
VAL_ADT: {"type.spec";} 'adt';
VAL_PROVER_TYPE: {"type.spec";} 'prover_type';
VAL_PROVER_FUNCTION: {"type.spec";} 'prover_function';

VAL_MODIFIES: {"keyword.spec";} 'modifies';
VAL_ACCESSIBLE: {"keyword.spec";} 'accessible';
VAL_REQUIRES: {"keyword.spec";} 'requires';
VAL_ENSURES: {"keyword.spec";} 'ensures';
VAL_CONTEXT_EVERYWHERE: {"keyword.spec";} 'context_everywhere';
VAL_CONTEXT: {"keyword.spec";} 'context';
VAL_LOOP_INVARIANT: {"keyword.spec";} 'loop_invariant';
VAL_KERNEL_INVARIANT: {"keyword.spec";} 'kernel_invariant';
VAL_LOCK_INVARIANT: {"keyword.spec";} 'lock_invariant';
VAL_SIGNALS: {"keyword.spec";} 'signals';
VAL_DECREASES: {"keyword.spec";} 'decreases';

VAL_APPLY: {"keyword.spec";} 'apply';
VAL_FOLD: {"keyword.spec";} 'fold';
VAL_UNFOLD: {"keyword.spec";} 'unfold';
VAL_OPEN: {"keyword.spec";} 'open';
VAL_CLOSE: {"keyword.spec";} 'close';
VAL_ASSUME: {"keyword.spec";} 'assume';
VAL_INHALE: {"keyword.spec";} 'inhale';
VAL_EXHALE: {"keyword.spec";} 'exhale';
VAL_LABEL: {"keyword.spec";} 'label';
VAL_EXTRACT: {"keyword.spec";} 'extract';
VAL_FRAME: {"keyword.spec";} 'frame';
VAL_OUTLINE: {"keyword.spec";} 'outline';
VAL_REFUTE: {"keyword.spec";} 'refute';
VAL_WITNESS: {"keyword.spec";} 'witness';
VAL_GHOST: {"keyword.spec";}'ghost';
VAL_SEND: {"keyword.spec";}'send';
VAL_WORD_TO: {"keyword.spec";} 'to';
VAL_RECV: {"keyword.spec";} 'recv';
VAL_FROM: {"keyword.spec";} 'from';
VAL_TRANSFER: {"keyword.spec";} 'transfer';
VAL_CSL_SUBJECT: {"keyword.spec";} 'csl_subject';
VAL_SPEC_IGNORE: {"keyword.spec";} 'spec_ignore';
VAL_SPEC_REPLACE_EXPR_DONE: {"keyword.spec";} '\\replacing_done';
VAL_SPEC_REPLACE_EXPR: {"keyword.spec";} '\\replacing';
VAL_ACTION: {"keyword.spec";} 'action';
VAL_ATOMIC: {"keyword.spec";} 'atomic';
VAL_COMMIT: {"keyword.spec";} 'commit';

VAL_REDUCIBLE: {"keyword.spec";} 'Reducible';
VAL_ADDS_TO: {"keyword.spec";} 'AddsTo';
VAL_APERM: {"keyword.spec";} 'APerm';
VAL_ARRAYPERM: {"keyword.spec";} 'ArrayPerm';
VAL_CONTRIBUTION: {"keyword.spec";} 'Contribution';
VAL_HELD: {"keyword.spec";} 'held';
VAL_COMMITTED: {"keyword.spec";} 'committed';
VAL_HPERM: {"keyword.spec";} 'HPerm';
VAL_IDLE: {"keyword.spec";} 'idle';
VAL_PERM_VAL: {"keyword.spec";} 'perm';
VAL_PERM: {"keyword.spec";} 'Perm';
VAL_POINTS_TO: {"keyword.spec";} 'PointsTo';
VAL_RUNNING: {"keyword.spec";} 'running';
VAL_SOME: {"keyword.spec";}  'Some';
VAL_LEFT: {"keyword.spec";} 'Left';
VAL_RIGHT: {"keyword.spec";} 'Right';
VAL_VALUE: {"keyword.spec";} 'Value';

UNFOLDING: {"keyword.spec";} '\\unfolding';
UNFOLDING_JAVA: {"keyword.spec";} '\\Unfolding';
IN: {"keyword.spec";} '\\in';
MEMBEROF: {"keyword.spec";} '\\memberof';
CURRENT_THREAD: {"keyword.spec";} '\\current_thread';
FORALL_STAR: {"keyword.spec";} '\\forall*';
FORALL: {"keyword.spec";} '\\forall';
EXISTS: {"keyword.spec";} '\\exists';
FORPERM: {"keyword.spec";} '\\forperm';
FORALL_UNICODE: '\u2200';
FORALL_STAR_UNICODE: '\u2200*';
EXISTS_UNICODE: '\u2203';
LET: {"keyword.spec";} '\\let';
SUM: {"keyword.spec";} '\\sum';
LENGTH: {"keyword.spec";} '\\length';
OLD: {"keyword.spec";} '\\old';
TYPEOF: {"keyword.spec";} '\\typeof';
TYPEVALUE: {"keyword.spec";} '\\type';
MATRIX: {"keyword.spec";} '\\matrix';
ARRAY: {"keyword.spec";} '\\array';
POINTER: {"keyword.spec";} '\\pointer';
POINTER_INDEX: {"keyword.spec";} '\\pointer_index';
POINTER_BLOCK_LENGTH: {"keyword.spec";} '\\pointer_block_length';
POINTER_BLOCK_OFFSET: {"keyword.spec";} '\\pointer_block_offset';
POINTER_LENGTH: {"keyword.spec";} '\\pointer_length';
SHARED_MEM_SIZE: {"keyword.spec";} '\\shared_mem_size';
VALUES: {"keyword.spec";} '\\values';
VCMP: {"keyword.spec";} '\\vcmp';
VREP: {"keyword.spec";} '\\vrep';
MSUM: {"keyword.spec";} '\\msum';
MCMP: {"keyword.spec";} '\\mcmp';
MREP: {"keyword.spec";} '\\mrep';
RESULT: {"keyword.spec";} '\\result';
LTID: {"keyword.spec";} '\\ltid';
GTID: {"keyword.spec";} '\\gtid';
VAL_INDEX: {"keyword.spec";} '\\nd_index';
VAL_LENGTH: {"keyword.spec";} '\\nd_length';
VAL_PARTIAL_INDEX: {"keyword.spec";} '\\nd_partial_index';
POLARITY_DEPENDENT: {"keyword.spec";} '\\polarity_dependent';
SMT_LIB: {"keyword.spec";} '\\smtlib';
BOOGIE: {"keyword.spec";} '\\boogie';

NONE: {"keyword.spec";} 'none';
OPTION_NONE: {"keyword.spec";} 'None';
WRITE: {"keyword.spec";} 'write';
READ: {"keyword.spec";} 'read';
EMPTY: {"keyword.spec";} 'empty';

COALESCE: '?.';
FRAC_DIV: '\\';
SEP_CONJ: '**';
IMPLIES: '==>';
WAND: '-*';
RANGE_TO: '..';
TRIGGER_OPEN: '{:' ('<'* [0-9]* ':')?;
TRIGGER_CLOSE: ':}';
LITERAL_BAG_OPEN: 'b{';
EMPTY_SEQ_OPEN: '[t:';
EMPTY_SET_OPEN: '{t:';
EMPTY_BAG_OPEN: 'b{t:';
ARROW_LEFT: '<-';

VAL_EXPECT_ERROR_OPEN: '/*'? '[/expect ' [a-zA-Z:]+ ']' '*/'? -> channel(EXPECTED_ERROR_CHANNEL);
VAL_EXPECT_ERROR_CLOSE: '/*'? '[/end]' '*/'? -> channel(EXPECTED_ERROR_CHANNEL);

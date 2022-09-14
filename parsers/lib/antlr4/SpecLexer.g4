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
VAL_INLINE: 'inline';
VAL_ASSERT: 'assert';
VAL_TRUE: 'true';
VAL_FALSE: 'false';
VAL_PACKAGE: 'package';
*/

// Must be able to contain identifiers from any frontend, so it's fine to over-approximate valid identifiers a bit.
LANG_ID_ESCAPE: '`' ~[`]+ '`';

VAL_RESOURCE: 'resource';
VAL_PROCESS: 'process';
VAL_FRAC: 'frac';
VAL_ZFRAC: 'zfrac';
VAL_BOOL: 'bool';
VAL_REF: 'ref';
VAL_RATIONAL: 'rational';
VAL_SEQ: 'seq';
VAL_SET: 'set';
VAL_BAG: 'bag';
VAL_POINTER: 'pointer';
VAL_MAP: 'map';
VAL_OPTION: 'option';
VAL_EITHER: 'either';
VAL_TUPLE: 'tuple';
VAL_TYPE: 'type';
VAL_ANY: 'any';
VAL_NOTHING: 'nothing';

VAL_PURE: 'pure';
VAL_THREAD_LOCAL: 'thread_local';

VAL_WITH: 'with';
VAL_THEN: 'then';
VAL_GIVEN: 'given';
VAL_YIELDS: 'yields';

VAL_AXIOM: 'axiom';
VAL_MODEL: 'model';
VAL_ADT: 'adt';

VAL_MODIFIES: 'modifies';
VAL_ACCESSIBLE: 'accessible';
VAL_REQUIRES: 'requires';
VAL_ENSURES: 'ensures';
VAL_CONTEXT_EVERYWHERE: 'context_everywhere';
VAL_CONTEXT: 'context';
VAL_LOOP_INVARIANT: 'loop_invariant';
VAL_KERNEL_INVARIANT: 'kernel_invariant';
VAL_LOCK_INVARIANT: 'lock_invariant';
VAL_SIGNALS: 'signals';
VAL_DECREASES: 'decreases';

VAL_APPLY: 'apply';
VAL_FOLD: 'fold';
VAL_UNFOLD: 'unfold';
VAL_OPEN: 'open';
VAL_CLOSE: 'close';
VAL_ASSUME: 'assume';
VAL_INHALE: 'inhale';
VAL_EXHALE: 'exhale';
VAL_LABEL: 'label';
VAL_REFUTE: 'refute';
VAL_WITNESS: 'witness';
VAL_GHOST: 'ghost';
VAL_SEND: 'send';
VAL_WORD_TO: 'to';
VAL_RECV: 'recv';
VAL_FROM: 'from';
VAL_TRANSFER: 'transfer';
VAL_CSL_SUBJECT: 'csl_subject';
VAL_SPEC_IGNORE: 'spec_ignore';
VAL_ACTION: 'action';
VAL_ATOMIC: 'atomic';

VAL_REDUCIBLE: 'Reducible';
VAL_ADDS_TO: 'AddsTo';
VAL_APERM: 'APerm';
VAL_ARRAYPERM: 'ArrayPerm';
VAL_CONTRIBUTION: 'Contribution';
VAL_HELD: 'held';
VAL_HPERM: 'HPerm';
VAL_IDLE: 'idle';
VAL_PERM_VAL: 'perm';
VAL_PERM: 'Perm';
VAL_POINTS_TO: 'PointsTo';
VAL_RUNNING: 'running';
VAL_SOME: 'Some';
VAL_LEFT: 'Left';
VAL_RIGHT: 'Right';
VAL_VALUE: 'Value';

UNFOLDING: '\\unfolding';
IN: '\\in';
MEMBEROF: '\\memberof';
CURRENT_THREAD: '\\current_thread';
FORALL_STAR: '\\forall*';
FORALL: '\\forall';
EXISTS: '\\exists';
FORALL_UNICODE: '\u2200';
FORALL_STAR_UNICODE: '\u2200*';
EXISTS_UNICODE: '\u2203';
LET: '\\let';
SUM: '\\sum';
LENGTH: '\\length';
OLD: '\\old';
TYPEOF: '\\typeof';
TYPEVALUE: '\\type';
MATRIX: '\\matrix';
ARRAY: '\\array';
POINTER: '\\pointer';
POINTER_INDEX: '\\pointer_index';
POINTER_BLOCK_LENGTH: '\\pointer_block_length';
POINTER_BLOCK_OFFSET: '\\pointer_block_offset';
POINTER_LENGTH: '\\pointer_length';
SHARED_MEM_SIZE: '\\shared_mem_size';
VALUES: '\\values';
VCMP: '\\vcmp';
VREP: '\\vrep';
MSUM: '\\msum';
MCMP: '\\mcmp';
MREP: '\\mrep';
RESULT: '\\result';
LTID: '\\ltid';
GTID: '\\gtid';

NONE: 'none';
OPTION_NONE: 'None';
WRITE: 'write';
READ: 'read';
EMPTY: 'empty';

COALESCE: '?.';
CONS: '::';
FRAC_DIV: '\\';
SEP_CONJ: '**';
IMPLIES: '==>';
WAND: '-*';
RANGE_TO: '..';
TRIGGER_OPEN: '{:';
TRIGGER_CLOSE: ':}';
LITERAL_BAG_OPEN: 'b{';
EMPTY_SEQ_OPEN: '[t:';
EMPTY_SET_OPEN: '{t:';
EMPTY_BAG_OPEN: 'b{t:';
ARROW_LEFT: '<-';

VAL_EXPECT_ERROR_OPEN: '/*'? '[/expect ' [a-zA-Z:]+ ']' '*/'? -> channel(EXPECTED_ERROR_CHANNEL);
VAL_EXPECT_ERROR_CLOSE: '/*'? '[/end]' '*/'? -> channel(EXPECTED_ERROR_CHANNEL);
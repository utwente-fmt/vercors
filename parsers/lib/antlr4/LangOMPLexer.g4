lexer grammar LangOMPLexer;

fragment OMP_WS
    : [ \t]+
    ;

OMP_PRAGMA
    : '#' OMP_WS? 'pragma' OMP_WS 'omp' -> mode(OMP), skip
    ;

mode OMP;
OMP_PRAGMA_END
    : ('\n' | '\r\n') -> mode(DEFAULT_MODE), skip;

OMP_WHITESPACE
    : OMP_WS -> skip;

/* There's a bunch of string literals in the C Parser (e.g. 'static'), which ANTLR must resolve to a proper lexer token.
 * Since OMP string literals overlap with C string literals, we must convince ANTLR to pick the C tokens. Putting
 * parens around the token is sufficient for the token not to be recognized as a string constant. */
OMP_PAREN_OPEN: ('(');
OMP_PAREN_CLOSE: (')');
OMP_COMMA: (',');
OMP_COLON: (':');

OMP_PARALLEL: 'parallel';
OMP_FOR: ('for');
OMP_SECTION: 'section';
OMP_SECTIONS: 'sections';

OMP_NOWAIT: 'nowait';
OMP_PRIVATE: 'private';
OMP_SHARED: 'shared';
OMP_SCHEDULE: 'schedule';
OMP_STATIC: ('static');
OMP_SIMD: 'simd';
OMP_SIMDLEN: 'simdlen';
OMP_NUMTHREADS: 'num_threads';
OMP_REDUCTION: 'reduction'; // TODO

OMP_REDUCTION_OP: [+\-*&|^] | '&&' | '||';
OMP_IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;
OMP_POSITIVE_INTEGER: [1-9][0-9]*;
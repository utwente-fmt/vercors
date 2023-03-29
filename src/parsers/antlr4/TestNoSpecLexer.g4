lexer grammar TestNoSpecLexer;

/* TestNoSpec{Lexer,Parser} exist to test whether the lexer of a frontend is complete. The parser may use tokens from
 * the spec lexer. If that happens, we may allow important keywords in the frontend (say "assert") to be used via the
 * valReserved identifier escape hatch, which is undesired. */
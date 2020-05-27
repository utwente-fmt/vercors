grammar Binder;

top: line* EOF;
line: '[binder]' expr '\n';

expr
    : INT
    | ID
    | ID '{' arguments? '}'
    | expr '.' ID
    | expr '[' expr ']'
    | expr '(' arguments? ')'
    | '[' expr ']' expr
    | '(' '\\forall*' decls trigger? ';' expr ';' expr ')'
    | '(' '\\forall' decls trigger? ';' expr ';' expr ')'
    | '(' '\\exists' decls trigger? ';' expr ';' expr ')'
    | '(' expr '\\memberof' expr ')'
    | '(' expr ')'
    | '|' expr '|'
    | ('-'|'!') expr
    | expr ('*'|'/'|'\\'|'%') expr
    | expr ('+'|'-') expr
    | expr ('<'|'<='|'>='|'>') expr
    | expr ('!='|'==') expr
    | expr ('&&'|'||') expr
    | expr '==>' expr
    | expr '?' expr ':' expr
    ;

trigger
    : '{' arguments '}'
    ;

arguments
    : expr
    | expr ',' arguments
    ;

decls
    : decl
    | decl ',' decls
    ;

decl
    : type identifier
    ;

type
    : identifier
    ;

identifier
    : ID
    ;

ID: [$a-zA-Z_\\][$a-zA-Z0-9_<>]*;
INT: [0-9]+;
WHITESPACE: [ \t]+ -> skip;

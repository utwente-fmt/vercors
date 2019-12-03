grammar Java8JML;

import val, Java8;

identifier : javaIdentifier ;

extraIdentifier : valReserved ;

extraType : 'resource' | 'process' | 'frac' | 'zfrac' | identifier typeArgs ;

typeArgs : '<' ((expression | type) (',' (expression | type))*)? '>' ;


extraAnnotation
    : 'pure'
    | 'inline'
    | 'thread_local'
    ;

extraStatement
    : 'with' block // not really a statement.
    | 'then' block // not really a statement.
    | 'given' localVariableDeclaration ';' // makes T x,y; possible
    | 'yields' localVariableDeclaration ';' // makes T x,y; possible
    | valContractClause
    | valStatement
    ;

extraPrimary
    : Identifier ':' expression // used for witness labels
    | valPrimary
    ;

expressionList
    :   labeledExpression (',' labeledExpression)*
    ;

labeledExpression
    : ( Identifier ':' )? expression
    ;

extraDeclaration
    : functionDeclaration
    | axiomDeclaration
    ;

functionDeclaration
    : methodModifier* methodHeader '=' expression ';'
    ;

axiomDeclaration
    : 'axiom' Identifier '{' expression '==' expression '}'
    ;

specificationSequence : ( classBodyDeclaration | statement )* ;

EmbeddedLatex
    : '#' ~[\r\n]* '#' -> skip
    ;

FileName : '"' ~[\r\n"]* '"' ;

LINEDIRECTION
    :   '#' WS? IntegerLiteral WS? FileName ~[\r\n]* -> channel(2)
    ;

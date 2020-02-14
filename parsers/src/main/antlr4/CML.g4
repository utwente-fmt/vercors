grammar CML;

import val, C;

/* Define for use by C grammar */

extraIdentifier : valReserved ;

extraPrimary : valPrimary ;

extraStatement : valContractClause | valStatement ;

extraType : 'resource' | 'process' | 'frac' | 'zfrac' | 'bool' | identifier typeArgs ;

extraDeclaration
 : pureFunctionDeclaration
 ;

/* auxiliary defs */

typeArgs : '<' ((expression | type) (',' (expression | type))*)? '>' ;

pureFunctionDeclaration
    : declarationSpecifiers declarator '=' expression ';'
    ;

/* Define for use by val grammar */

type : typeSpecifier ;

identifier : clangIdentifier ;

block : compoundStatement ;

// expression already defined by C grammar

/* Define for use by comment parser */

specificationSequence : ( externalDeclaration | statement | valInvocationAnnotation)* ;

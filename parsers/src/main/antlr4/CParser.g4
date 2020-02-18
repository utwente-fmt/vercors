parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

/* Define for use by C grammar */

extraIdentifier : valReserved;

extraPrimary : valPrimary;

extraStatement : valEmbedStatementBlock;

extraType : 'resource' | 'process' | 'frac' | 'zfrac' | 'bool' | identifier typeArgs ;

extraDeclaration
 : pureFunctionDeclaration
 ;

startSpec: LineStartSpec | BlockStartSpec;
endSpec: EndSpec;

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

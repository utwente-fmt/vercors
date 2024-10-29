parser grammar PVLParser;
options {tokenVocab = LangPVLLexer;}
import LangPVLParser, SpecParser;

langExpr: expr;
langId: identifier;
langConstInt: NUMBER;
langType: type;
langStatement: statement;
langStatic: 'static';
langGlobalDecl: NEVER;
langClassDecl: NEVER;
specTrue: 'true';
specFalse: 'false';

startSpec: NEVER;
endSpec: NEVER;
parser grammar PVLParser;
options {tokenVocab = LangPVLLexer;}
import LangPVLParser, SpecParser;

langExpr: expr;
langId: identifier;
langType: type;
langStatement: statement;
langStatic: 'static';
langGlobalDecl: EOF EOF;
langClassDecl: EOF EOF;

startSpec: EOF EOF;
endSpec: EOF EOF;
parser grammar PVLParser;
options {tokenVocab = LangPVLLexer;}
import LangPVLParser, SpecParser;

langExpr: expr;
langId: identifier;
langType: type;
langModifier: modifier;
langStatement: statement;
langDecl: EOF EOF;

startSpec: EOF EOF;
endSpec: EOF EOF;
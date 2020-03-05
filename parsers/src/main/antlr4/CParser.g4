parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

@parser::members {
    private static int ghostLevel = 0;
}

langExpr: expression;
langId: clangIdentifier;
langType: typeSpecifier;
langModifier: storageClassSpecifier;
langStatement: statement;

startSpec: LineStartSpec | BlockStartSpec;
endSpec: EndSpec;

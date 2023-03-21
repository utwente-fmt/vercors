parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: clangIdentifier;
langConstInt: Constant;
langType: typeSpecifier;
langStatement: blockItem;
langStatic: EOF EOF;
langGlobalDecl: externalDeclaration;
langClassDecl: EOF EOF;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};

parser grammar CPPParser;
options {tokenVocab = LangCPPLexer;}
import LangCPPParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: assignmentExpression;
langId: clangppIdentifier;
langConstInt: literal;
langType: typeSpecifier;
langStatement: statement;
langStatic: EOF EOF;
langGlobalDecl: declaration;
langClassDecl: EOF EOF;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};

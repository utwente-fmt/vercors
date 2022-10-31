parser grammar CPPParser;
options {tokenVocab = LangCPPLexer; }
import LangCPPParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: cppIdentifier;
langType: typeSpecifier;
langModifier: storageClassSpecifier;
langStatement: statement;
langDecl: declaration;

langConstInt: Constant;
langStatic: EOF EOF;
langGlobalDecl: declaration;
langClassDecl: EOF EOF;




startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};


parser grammar LLVMSpecParser;
options {tokenVocab = LangLLVMSpecLexer;}
import LangLLVMSpecParser, SpecParser;


@parser::members {
    public int specLevel = 1;
}

langExpr: expression;
langId: Identifier;
langConstInt: Constant;
langType: type;
langStatement: EOF EOF;
langStatic: EOF EOF;
langGlobalDecl: EOF EOF;
langClassDecl: EOF EOF;

startSpec: StartSpec {specLevel++;};
endSpec: EndSpec {specLevel--;};

parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: clangIdentifier;
langType: typeSpecifier;
langModifier: storageClassSpecifier;
langStatement: blockItem;
langDecl: functionDefinition;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};

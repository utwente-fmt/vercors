parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: clangIdentifier;
langType: typeSpecifier | {specLevel>0}? valType;
langModifier: storageClassSpecifier;
langStatement: blockItem;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;};
endSpec: EndSpec {specLevel--;};

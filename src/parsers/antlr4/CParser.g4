parser grammar CParser;
options {tokenVocab = LangCLexer;}
import LangCParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: assignmentExpression;
langId: clangIdentifier;
langConstInt: Constant;
langType: typeSpecifierWithPointerOrArray;
langStatement: blockItem;
langStatic: NEVER;
langGlobalDecl: externalDeclaration;
langClassDecl: NEVER;
specTrue: 'true';
specFalse: 'false';

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};

typeSpecifierWithPointerOrArray : specifierQualifierList
    | specifierQualifierList '[' ']'
    | specifierQualifierList '*';
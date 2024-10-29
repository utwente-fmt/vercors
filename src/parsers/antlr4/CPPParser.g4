parser grammar CPPParser;
options {
    superClass = CPPParserBase;
    tokenVocab = LangCPPLexer;
}
import LangCPPParser, SpecParser;

@header {
    import vct.parsers.parser.CPPParserBase;
}
@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: clangppIdentifier;
langConstInt: literal;
langType: typeSpecifier;
langStatement: statement;
langStatic: NEVER;
langGlobalDecl: declaration;
langClassDecl: NEVER;
valArg: parameterDeclaration;
specTrue: NEVER;
specFalse: NEVER;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};

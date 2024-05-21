parser grammar JavaParser;
options {tokenVocab = LangJavaLexer;}
import LangJavaParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: javaIdentifier;
langConstInt: IntegerLiteral;
langType: type;
langStatement: blockStatement;
langStatic: 'static';
langClassDecl: classBodyDeclaration;
langGlobalDecl: typeDeclaration;
specTrue: NEVER;
specFalse: NEVER;

startSpec
    : LineStartSpec {specLevel++;}
    | BlockStartSpec {specLevel++;}
    | BlockStartSpecImmediate {specLevel++;}
    ;

endSpec
    : EndSpec {specLevel--;}
    | LineCommentStartInSpec {specLevel--;}
    ;
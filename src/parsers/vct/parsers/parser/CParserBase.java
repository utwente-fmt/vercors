package vct.parsers.parser;

import org.antlr.v4.runtime.*;
import vct.antlr4.generated.CParser;

import java.util.HashSet;
import java.util.Set;

public abstract class CParserBase extends Parser {
    private final Set<String> typedefNames = new HashSet<>();

    protected CParserBase(TokenStream input) {
        super(input);
    }

    protected void collectTypedef() {
        if (_ctx instanceof CParser.Declaration0Context declarationContext &&
                declarationContext.declarationSpecifiers() instanceof CParser.DeclarationSpecifiers0Context specifiersContext &&
                specifiersContext.declarationSpecifier(0) instanceof CParser.DeclarationSpecifier0Context storageContext &&
                storageContext.storageClassSpecifier() instanceof CParser.StorageClassSpecifier0Context &&
                declarationContext.initDeclaratorList() instanceof CParser.InitDeclaratorList0Context declaratorsContext &&
                declaratorsContext.initDeclarator() instanceof CParser.InitDeclarator0Context initDeclaratorContext &&
                initDeclaratorContext.declarator() instanceof CParser.Declarator0Context declaratorContext) {
            if (declaratorContext.directDeclarator() instanceof CParser.DirectDeclarator0Context directDeclaratorContext) {
                if (directDeclaratorContext.clangIdentifier() instanceof CParser.ClangIdentifier0Context identifierContext) {
                    typedefNames.add(identifierContext.Identifier().getText());
                }
            } else if (declaratorContext.directDeclarator() instanceof CParser.DirectDeclarator5Context directDeclaratorContext) {
                if (directDeclaratorContext.directDeclarator() instanceof CParser.DirectDeclarator0Context directDeclaratorContext2) {
                    if (directDeclaratorContext2.clangIdentifier() instanceof CParser.ClangIdentifier0Context identifierContext) {
                        typedefNames.add(identifierContext.Identifier().getText());
                    }
                }
            } else if (declaratorContext.directDeclarator() instanceof CParser.DirectDeclarator6Context directDeclaratorContext) {
                if (directDeclaratorContext.directDeclarator() instanceof CParser.DirectDeclarator0Context directDeclaratorContext2) {
                    if (directDeclaratorContext2.clangIdentifier() instanceof CParser.ClangIdentifier0Context identifierContext) {
                        typedefNames.add(identifierContext.Identifier().getText());
                    }
                }
            }
        }
    }

    protected boolean isTypedefName(ParserRuleContext ctx) {
        if (ctx instanceof CParser.CastExpressionContext) {
            int openParenthesisIndex = getCurrentToken().getTokenIndex();
            Token nameToken = getTokenStream().get(openParenthesisIndex+1);
            if (nameToken.getType() == CParser.Identifier && getTokenStream().get(openParenthesisIndex+2).getType() == CParser.RightParen) {
                return typedefNames.contains(nameToken.getText());
            } else {
                return true;
            }
        }
        return typedefNames.contains(ctx.start.getText());
    }
}

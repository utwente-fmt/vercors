package vct.col.ast.expr;


import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.type.Type;

public class SetComprehension extends BindingExpression {

    public  ASTNode[] boundedVariables;

    /**
     *
     * @param result_type
     * @param decls
     * @param selector
     * @param main
     * @param boundedVariables
     */
    public SetComprehension(Type result_type, DeclarationStatement[] decls, ASTNode selector, ASTNode main, ASTNode[] boundedVariables) {
        super(Binder.SetComp, result_type, decls, new ASTNode[0][], selector, main);
        this.boundedVariables = boundedVariables;
    }
}

package vct.col.ast.expr;


import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.type.Type;

import java.util.Map;

public class SetComprehension extends BindingExpression {

    private Map<NameExpression, ASTNode> variables;

    /**
     *  @param result_type
     * @param decls
     * @param selector
     * @param main
     * @param variables
     */
    public SetComprehension(Type result_type, DeclarationStatement[] decls, ASTNode selector, ASTNode main, Map<NameExpression, ASTNode> variables) {
        super(Binder.SetComp, result_type, decls, new ASTNode[0][], selector, main);
        this.variables = variables;
    }

    public Map<NameExpression, ASTNode> variables() {
        return this.variables;
    }
}

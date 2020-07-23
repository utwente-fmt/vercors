package vct.col.util;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;

public class AstToId extends RecursiveVisitor<String> {
    public static String toId(ASTNode node) {
        AstToId astToId = new AstToId();
        return node.apply(astToId);
    }

    private AstToId() {
        super((ProgramUnit) null);
    }

    public void visit(Dereference e) {
        super.visit(e);
        result = getResult() + "_" + e.field();
    }

    public void visit(NameExpression n) {
        super.visit(n);
        result = n.getName();
    }
}

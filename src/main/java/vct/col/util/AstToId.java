package vct.col.util;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.FieldAccess;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;

public class AstToId extends RecursiveVisitor<String> {
    private AstToId() {
        super((ProgramUnit) null);
    }

    public static String toId(ASTNode node) {
        AstToId astToId = new AstToId();
        return node.apply(astToId);
    }

    public void visit(Dereference e) {
        e.obj().accept(this);
        result = getResult() + "_" + e.field();
    }

    public void visit(NameExpression n) {
        result = n.getName();
    }

    public void visit(FieldAccess f) {
        f.object().accept(this);
        result = getResult() + "_" + f.name();

        if (f.value() != null) {
            String localResult = result;
            f.value().apply(this);
            result = localResult + getResult();
        }
    }
}

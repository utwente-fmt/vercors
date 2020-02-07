package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.util.ClassName;

import static vct.col.ast.type.PrimitiveSort.Map;

/**
 * This pass is meant for rewriting operators on ADTs that depend on the check pass.
 */
public class ADTOperatorRewriter extends AbstractRewriter {

    public ADTOperatorRewriter(ProgramUnit source) {
        super(source, true);
    }

    @Override
    public void visit(OperatorExpression e) {
        switch (e.operator()) {
            case EQ: {
                if (e.arg(0).getType().isPrimitive(Map) && e.arg(1).getType().isPrimitive(Map)) {
                    result = create.expression(StandardOperator.MapEquality, e.arg(0).apply(this), e.arg(1).apply(this));
                    return;
                }
                break;
            }
            case Subscript: {
                if (e.arg(0).getType().isPrimitive(Map)) {
                    result = create.expression(StandardOperator.MapGetByKey, e.arg(0).apply(this), e.arg(1).apply(this));
                    return;
                }
                break;
            }
            case Size: {
                if (e.arg(0).getType().isPrimitive(Map)) {
                    ASTNode e1 = rewrite(e.arg(0));
                    result = create.expression(StandardOperator.MapCardinality, e1);
                    return;
                }
                break;
            }
            default:
                super.visit(e);
        }
        // The condition here is to ensure that you do not overwrite anything done above
        if (result == null) {
            super.visit(e);
        }
    }
}

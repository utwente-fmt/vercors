package vct.col.rewrite;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import static vct.col.ast.type.PrimitiveSort.Map;

import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;

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
            case PrependSingle: {
                Type seqElementType = e.arg(0).getType();
                ASTNode var = e.arg(0).apply(this);
                ASTNode seq = e.arg(1).apply(this);

                StructValue newSeq = create.struct_value(create.primitive_type(PrimitiveSort.Sequence, seqElementType), null, var);
                result = create.expression(StandardOperator.Concat, newSeq, seq);

                break;
            }
            case AppendSingle: {
                Type seqElementType = e.arg(1).getType();
                ASTNode var = e.arg(1).apply(this);
                ASTNode seq = e.arg(0).apply(this);

                StructValue newSeq = create.struct_value(create.primitive_type(PrimitiveSort.Sequence, seqElementType), null, var);
                result = create.expression(StandardOperator.Concat, seq, newSeq);
                break;
            }
            case LTE:
            case LT:
                if (e.arg(0).getType().isPrimitive(PrimitiveSort.Set) || e.arg(0).getType().isPrimitive(PrimitiveSort.Bag)) {
                    StandardOperator op = (e.operator().equals(StandardOperator.LT)) ? StandardOperator.SubSet : StandardOperator.SubSetEq;
                    ASTNode e1 = e.arg(0).apply(this);
                    ASTNode e2 = e.arg(1).apply(this);
                    result = create.expression(op, e1, e2);
                } else {
                    super.visit(e);
                }
                break;
            case SeqPermutation:
                Type seqElementType = (Type) e.arg(0).getType().firstarg();
                ASTNode left = e.arg(0).apply(this);
                ASTNode right = e.arg(1).apply(this);

                String forallIndexName = "seq_perm_element_" + seqElementType;
                DeclarationStatement forallIndex = new DeclarationStatement(forallIndexName, seqElementType);
                result = and(
                        eq(size(left), size(right)),
                        create.forall(
                                create.constant(true),
                                eq(
                                        create.expression(StandardOperator.Member, name(forallIndexName), left),
                                        create.expression(StandardOperator.Member, name(forallIndexName), right)),
                                forallIndex
                        )
                );
                break;

            case EQ: {
                if (e.arg(0).getType().isPrimitive(Map) && e.arg(1).getType().isPrimitive(Map)) {
                    result = create.expression(StandardOperator.MapEquality, e.arg(0).apply(this), e.arg(1).apply(this));
                } else {
                    super.visit(e);
                }
                break;
            }
            case Subscript: {
                if (e.arg(0).getType().isPrimitive(Map)) {
                    result = create.expression(StandardOperator.MapGetByKey, e.arg(0).apply(this), e.arg(1).apply(this));
                } else {
                    super.visit(e);
                }
                break;
            }
            case Size: {
                if (e.arg(0).getType().isPrimitive(Map)) {
                    ASTNode e1 = rewrite(e.arg(0));
                    result = create.expression(StandardOperator.MapCardinality, e1);
                } else {
                    super.visit(e);
                }
                break;
            }
            default:
                super.visit(e);
        }

    }
}

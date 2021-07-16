package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

public class FlattenBeforeAfter extends AbstractRewriter {

    public FlattenBeforeAfter(ProgramUnit source) {
        super(source);
    }


    @Override
    public void visit(BlockStatement s) {
        BlockStatement res = create.block();
        for (ASTNode item : s) {
            ASTNode tmp = rewrite(item);
            if ((tmp instanceof BeforeAfterAnnotations) && !(tmp instanceof MethodInvokation)) {
                BeforeAfterAnnotations baa = (BeforeAfterAnnotations) tmp;
                BlockStatement before = baa.get_before();
                BlockStatement after = baa.get_after();
                baa.set_before(null);
                baa.set_after(null);

                if (before != null) for (ASTNode n : before) {
                    n.clearParent();
                    res.add(n);
                }
                res.add(tmp);
                if (after != null) for (ASTNode n : after) {
                    n.clearParent();
                    res.add(n);
                }
            } else {
                res.add(tmp);
            }
        }
        result = res;
    }
}

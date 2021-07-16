package vct.col.util;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.ast.WrappingOrigin;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;

public class OriginWrapper extends RecursiveVisitor<Object> {

    public final WrappingOrigin base;

    public OriginWrapper(ProgramUnit source, WrappingOrigin base) {
        super(source);
        this.base = base;
    }

    public static void wrap(ProgramUnit source, ASTNode n, WrappingOrigin base) {
        OriginWrapper wrapper = new OriginWrapper(source, base);
        n.accept(wrapper);
    }

    @Override
    public void enter(ASTNode n) {
        super.enter(n);
        Origin o = n.getOrigin();
        if (o != null) {
            n.clearOrigin();
            n.setOrigin(base.wrap(o));
        } else {
            n.setOrigin(base.wrap(new MessageOrigin("unknown origin")));
        }
    }
}

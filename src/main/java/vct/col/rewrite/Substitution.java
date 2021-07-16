package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import java.util.HashMap;
import java.util.Map;

public class Substitution extends AbstractRewriter {
    public boolean copy = true;
    Map<NameExpression, ASTNode> map;

    public Substitution(ProgramUnit source, Map<NameExpression, ASTNode> map) {
        super(source);
        this.map = map;
    }

    public Substitution(ProgramUnit source) {
        this(source, new HashMap<>());
    }

    public void visit(NameExpression e) {
        ASTNode res = map.get(e);
        if (res == null) {
            super.visit(e);
        } else if (copy) {
            result = res.apply(copy_rw);
        } else {
            result = res;
        }
    }

    public Substitution subst(NameExpression name, ASTNode node) {
        map.put(name, node);
        return this;
    }
}

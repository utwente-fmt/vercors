package vct.parsers.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.Syntax;
import vct.col.ast.util.AbstractRewriter;

public class SpecificationCollector extends AbstractRewriter {
    private final Syntax syntax;

    public SpecificationCollector(Syntax syntax, ProgramUnit source) {
        super(source);
        this.syntax = syntax;
    }


    @Override
    public void visit(MethodInvokation m){
        StandardOperator op = syntax.parseFunction(m.method());
        if(op!=null && op.arity()==m.getArity()) {
            result = create.expression(op,rewrite(m.getArgs()));
        } else {
            super.visit(m);
        }
    }
}

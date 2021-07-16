package vct.experiments.learn;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.*;

public class TypesCountVisitor extends SpecialCountVisitor {

    public TypesCountVisitor(ProgramUnit pu) {
        super(pu);
    }

    @Override
    public void visit(ClassType ct) {
        specialCounter.count(ct.getClass().getName());
        super.visit(ct);
    }

    @Override
    public void visit(FunctionType ft) {
        specialCounter.count(ft.getClass().getName());
        super.visit(ft);
    }

    @Override
    public void visit(PrimitiveType pt) {
        specialCounter.count(pt.getClass().getName(), pt.sort.toString());
        super.visit(pt);
    }

    @Override
    public void visit(RecordType rt) {
        specialCounter.count(rt.getClass().getName());
        super.visit(rt);
    }

    @Override
    public void visit(TupleType tt) {
        specialCounter.count(tt.getClass().getName());
        super.visit(tt);
    }

    @Override
    public void visit(TypeExpression te) {
        specialCounter.count(te.getClass().getName());
        super.visit(te);
    }
}

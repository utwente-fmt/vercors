package vct.col.ast.util;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.FieldAccess;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.RecursiveVisitor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class FieldAccessCollector extends RecursiveVisitor<Object> {

    private List<Dereference> fieldAccesses = new ArrayList<>();

    public FieldAccessCollector() {
        super(null, null);
    }

    public List<Dereference> getFieldAccesses() {
        return fieldAccesses;
    }

    @Override
    public void visit(Dereference e) {
        fieldAccesses.add(e);
    }
}



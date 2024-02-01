package vct.parsers.transform.systemctocol.invariants.variables;

import vct.col.ast.Deref;
import vct.col.ast.Expr;
import vct.col.ast.Field;
import vct.col.ast.PVLDeref;

public abstract class ConcreteVariable<T> {

    public abstract boolean is(Expr<T> expression);

    public abstract boolean equals(ConcreteVariable<T> other);

    protected Field<T> extract_from_expression(Expr<T> expression) {
        if (expression instanceof Deref<T> deref) {
            return deref.ref().decl();
        }
        else throw new IllegalArgumentException("Could not parse expression " + expression.toString());
    }
}

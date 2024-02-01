package vct.parsers.transform.systemctocol.invariants.variables;

import vct.col.ast.Expr;
import vct.col.ast.Field;

public class FieldVariable<T> extends ConcreteVariable<T> {

    protected Field<T> field;

    public FieldVariable(Field<T> field) {
        this.field = field;
    }

    @Override
    public boolean is(Expr<T> expression) {
        return field.equals(extract_from_expression(expression));
    }

    @Override
    public boolean equals(ConcreteVariable<T> other) {
        if (other == this) return true;
        if (other instanceof FieldVariable<T> other_fv) {
            return this.field.equals(other_fv.field);
        }
        else return false;
    }
}

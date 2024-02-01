package vct.parsers.transform.systemctocol.invariants.variables;

import vct.col.ast.*;

public class SequenceIndexVariable<T> extends ConcreteVariable<T> {

    protected Field<T> sequence;
    protected int index;

    public SequenceIndexVariable(Field<T> sequence, int index) {
        this.sequence = sequence;
        this.index = index;
    }

    @Override
    public boolean is(Expr<T> expression) {
        if (expression instanceof AmbiguousSubscript<T> subscript) {
            return extract_from_expression(subscript.collection()).equals(sequence)
                    && resolve_index_expression(subscript.index()) == index;
        }
        else if (expression instanceof SeqSubscript<T> subscript) {
            return extract_from_expression(subscript.seq()).equals(sequence)
                    && resolve_index_expression(subscript.index()) == index;
        }
        else return false;
    }

    protected int resolve_index_expression(Expr<T> index_expression) {
        if (index_expression instanceof IntegerValue<T> integer_value) {
            return integer_value.value().intValue();
        }
        // TODO: Support simple arithmetic?
        else throw new IllegalArgumentException("Arithmetics in sequence accesses is not yet supported!");
    }

    @Override
    public boolean equals(ConcreteVariable<T> other) {
        if (other == this) return true;
        if (other instanceof SequenceIndexVariable<T> other_siv) {
            return this.sequence.equals(other_siv.sequence) && this.index == other_siv.index;
        }
        else return false;
    }
}

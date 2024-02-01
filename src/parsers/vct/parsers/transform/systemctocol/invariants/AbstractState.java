package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.Expr;
import vct.parsers.transform.systemctocol.invariants.variables.ConcreteVariable;

import java.util.HashMap;
import java.util.Map;

public class AbstractState<T> {
    private final Map<ConcreteVariable<T>, Integer> valuations;

    public AbstractState() {
        valuations = new HashMap<>();
    }

    public AbstractState(AbstractState<T> previous) {
        this.valuations = new HashMap<>(previous.valuations);
    }

    public boolean contains_variable(Expr<T> variable_expression) {
        return valuations.keySet().stream().anyMatch((var) -> var.is(variable_expression));
    }

    public void set_valuation(ConcreteVariable<T> var, int value) {
        valuations.put(var, value);
    }

    public int get_valuation(ConcreteVariable<T> var) {
        return valuations.get(var);
    }

    public int get_valuation(Expr<T> variable_expression) {
        return valuations.entrySet().stream()
                         .filter((entry) -> entry.getKey().is(variable_expression))
                         .findFirst().orElseThrow().getValue();
    }

    public boolean equals(AbstractState<T> other) {
        return valuations.equals(other.valuations);
    }
}

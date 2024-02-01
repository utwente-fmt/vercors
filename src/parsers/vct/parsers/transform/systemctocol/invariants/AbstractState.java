package vct.parsers.transform.systemctocol.invariants;

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

    public void set_valuation(ConcreteVariable<T> var, int value) {
        valuations.put(var, value);
    }

    public Integer get_valuation(ConcreteVariable<T> var) {
        return valuations.get(var);
    }

    public boolean equals(AbstractState<T> other) {
        return valuations.equals(other.valuations);
    }
}

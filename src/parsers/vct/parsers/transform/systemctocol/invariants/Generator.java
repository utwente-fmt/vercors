package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.InstanceMethod;
import vct.parsers.ParseResult;
import vct.parsers.transform.systemctocol.invariants.variables.ConcreteVariable;

import java.util.*;

public class Generator<T> {

    private final List<AbstractProcess<T>> processes;

    private final Set<ExecutableState<T>> active_branches;

    private final AbstractState<T> considered_state;

    public Generator(ParseResult<T> parse_result,
                     Map<ConcreteVariable<T>, Integer> considered_variables,
                     InstanceMethod<T> main_method) {
        // Initialize processes
        processes = new ArrayList<>();
        initialize_processes();

        // Initialize active branches
        active_branches = new HashSet<>();

        // Set initial state
        considered_state = new AbstractState<>();
    }

    public void execute() {

        initialize_branches();
    }

    private void execute_step() {

    }

    private void initialize_processes() {
        
    }

    private void initialize_branches() {

    }
}
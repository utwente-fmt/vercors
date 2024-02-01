package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.InstanceMethod;
import vct.parsers.ParseResult;
import vct.parsers.transform.systemctocol.invariants.variables.ConcreteVariable;

import java.util.*;

public class Generator<T> {

    private final List<AbstractProcess<T>> processes;

    private final List<ExecutableState<T>> active_branches;

    private final Set<ExecutableState<T>> considered_branches;

    public Generator(ParseResult<T> parse_result,
                     Map<ConcreteVariable<T>, Integer> considered_variables,
                     InstanceMethod<T> main_method) {
        // Initialize processes
        processes = new ArrayList<>();
        initialize_processes();

        // Initialize active branches
        active_branches = new ArrayList<>();

        // Initialize considered branches to empty list
        considered_branches = new HashSet<>();
    }

    public void execute() {
        initialize_branches();

        while (!active_branches.isEmpty()) {
            ExecutableState<T> exploring = active_branches.remove(0);
            List<ExecutableState<T>> new_possibilities = exploring.execute_step(processes)
                                                                  .stream()
                                                                  .filter((state) -> !considered_branches.contains(state))
                                                                  .toList();
            considered_branches.addAll(new_possibilities);
            active_branches.addAll(new_possibilities);
        }
    }

    private void initialize_processes() {
        
    }

    private void initialize_branches() {
        AbstractState<T> initial_state = new AbstractState<>();
    }
}
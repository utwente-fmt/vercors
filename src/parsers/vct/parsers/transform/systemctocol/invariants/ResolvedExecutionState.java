package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.Statement;

import java.util.ArrayList;
import java.util.List;

public class ResolvedExecutionState<T> {

    private AbstractState<T> state;
    private int[] program_counter;
    private List<Statement<T>> program_location;

    public ResolvedExecutionState(AbstractState<T> state, int[] program_counter, List<Statement<T>> program_location) {
        this.state = new AbstractState<>(state);
        this.program_counter = program_counter.clone();
        this.program_location = new ArrayList<>(program_location);
    }

    public AbstractState<T> get_state() {
        return state;
    }

    public int[] get_program_counter() {
        return program_counter;
    }

    public List<Statement<T>> get_program_location() {
        return program_location;
    }

    public Statement<T> get_current_statement() {
        return program_location.get(program_location.size() - 1);
    }

    public void move_forward() {
        
    }

    public ExecutionState<T> export() {
        return new ExecutionState<>(state, program_counter);
    }
}

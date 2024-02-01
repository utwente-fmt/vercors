package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.Statement;

public class ExecutionState<T> {
    private AbstractState<T> abstract_state;
    private Statement<T> next;
}

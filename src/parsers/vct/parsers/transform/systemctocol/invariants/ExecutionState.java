package vct.parsers.transform.systemctocol.invariants;

public record ExecutionState<T> (AbstractState<T> abstract_state, int[] program_counter) {}


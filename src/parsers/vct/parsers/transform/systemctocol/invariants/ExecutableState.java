package vct.parsers.transform.systemctocol.invariants;

public record ExecutableState<T> (AbstractState<T> state, int[][] execution_location_by_process, AbstractProcess<T> next_process) {}

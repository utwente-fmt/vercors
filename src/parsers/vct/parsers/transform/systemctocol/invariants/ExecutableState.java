package vct.parsers.transform.systemctocol.invariants;

import java.util.ArrayList;
import java.util.List;

public record ExecutableState<T> (AbstractState<T> state,
                                  int[][] execution_location_by_process,
                                  int next_process_id) {

    public List<ExecutableState<T>> execute_step(List<AbstractProcess<T>> processes) {
        AbstractProcess<T> next_process = processes.get(next_process_id);
        List<ExecutionState<T>> updates = next_process.simulate(execution_location_by_process[next_process_id], state);

        List<ExecutableState<T>> result = new ArrayList<>();
        for (ExecutionState<T> update : updates) {
            int[][] updated_counter = execution_location_by_process.clone();
            updated_counter[next_process_id] = update.program_counter();
            for (int i = 0; i < processes.size(); i++) {
                result.add(new ExecutableState<>(update.abstract_state(), updated_counter, i));
            }
        }
        return result;
    }
}

package vct.parsers.transform.systemctocol.colmodel;

import de.tub.pes.syscir.sc_model.SCFunction;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;

/**
 * A helper class for bookkeeping in the early phase of the encoding. Keeps track of attributes that are important for
 * the transformation of the process class later on and generates the unique process ID for each process class.
 */
public class ProcessClass extends COLClass {

    // ============================================================================================================== //
    // ======================================= CLASS ATTRIBUTES AND FUNCTIONS ======================================= //
    // ============================================================================================================== //

    /**
     * A class attribute keeping track of how many process classes have been created; used to assign each process a
     * globally unique ID.
     */
    private static int nr_processes = 0;

    public static int get_nr_processes() {
        return nr_processes;
    }

    // ============================================================================================================== //
    // ============================================================================================================== //
    // ============================================================================================================== //

    /**
     * The process method that is responsible for generating this class and should be encoded in its run method.
     */
    private final SCFunction run_method;

    /**
     * A globally unique ID for the process; index in the process_state sequence.
     */
    private final int process_id;

    public ProcessClass(SCClassInstance sc_inst, SCFunction run_method) {
        super(sc_inst);
        this.run_method = run_method;
        this.process_id = nr_processes++;
    }

    public SCFunction get_generating_function() {
        return run_method;
    }

    public int get_process_id() {
        return process_id;
    }

    public boolean equals(Object o) {
        if (!super.equals(o)) return false;
        ProcessClass other = (ProcessClass) o;

        return other.run_method.equals(run_method)
                && other.process_id == process_id;
    }
}

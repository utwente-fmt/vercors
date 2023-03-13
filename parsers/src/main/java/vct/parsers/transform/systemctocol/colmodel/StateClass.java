package vct.parsers.transform.systemctocol.colmodel;

import de.tub.pes.syscir.sc_model.variables.SCClassInstance;

/**
 * Helper class for bookkeeping early on during the encoding process. Represents the obligation to create a state class
 * without a run method later on.
 */
public class StateClass extends COLClass {

    public StateClass(SCClassInstance sc_inst) {
        super(sc_inst);
    }

    public boolean equals(Object o) {
        return super.equals(o);
    }
}

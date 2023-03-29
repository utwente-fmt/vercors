package vct.parsers.transform.systemctocol.colmodel;

import de.tub.pes.syscir.sc_model.SCFunction;
import de.tub.pes.syscir.sc_model.SCVariable;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;

/**
 * Helper class for bookkeeping during the early phases of the encoding. Represents an obligation to create a COL class
 * later on. Contains necessary information for the transformation, such as members of the corresponding SystemC class
 * to be transformed during the encoding process.
 */
public class COLClass {

    /**
     * SystemC class instance that triggered the creation of this class.
     */
    protected final SCClassInstance generating_instance;

    /**
     * Constructor of the SystemC class this class is based on. Possibly null if multiple classes are generated from the
     * original SystemC class, e.g. if the SystemC class defines multiple processes.
     */
    protected SCFunction sc_constructor;

    /**
     * A list of methods of the SystemC class that should be encoded in this class.
     */
    protected final java.util.List<SCFunction> methods_to_convert;

    /**
     * A list of attributes of the SystemC class that should be encoded in this class.
     */
    protected final java.util.List<SCVariable> attributes_to_convert;

    /**
     * Contains the number of automatically generated functions in this class.
     */
    protected int automatically_generated_functions;

    public COLClass(SCClassInstance sc_inst) {
        this.generating_instance = sc_inst;
        this.sc_constructor = null;
        this.methods_to_convert = new java.util.ArrayList<>();
        this.attributes_to_convert = new java.util.ArrayList<>();
        this.automatically_generated_functions = 0;
    }

    public SCClassInstance get_generating_instance() {
        return generating_instance;
    }

    public void set_constructor(SCFunction cons) {
        this.sc_constructor = cons;
    }

    public SCFunction get_constructor() {
        return sc_constructor;
    }

    public void add_methods(java.util.Collection<SCFunction> methods) {
        this.methods_to_convert.addAll(methods);
    }

    public java.util.List<SCFunction> get_methods() {
        return methods_to_convert;
    }

    public void add_attributes(java.util.Collection<SCVariable> vars) {
        this.attributes_to_convert.addAll(vars);
    }

    public java.util.List<SCVariable> get_attributes() {
        return attributes_to_convert;
    }

    public void add_generated_functions(int nr) {
        automatically_generated_functions += nr;
    }

    public int get_generated_functions() {
        return automatically_generated_functions;
    }

    public boolean equals(Object o) {
        if (o == null) return false;
        if (o == this) return true;
        if (o.getClass() != getClass()) return false;

        COLClass other = (COLClass) o;
        return other.generating_instance.equals(generating_instance)
                && other.sc_constructor.equals(sc_constructor)
                && other.methods_to_convert.equals(methods_to_convert)
                && other.attributes_to_convert.equals(attributes_to_convert)
                && other.automatically_generated_functions == automatically_generated_functions;
    }
}

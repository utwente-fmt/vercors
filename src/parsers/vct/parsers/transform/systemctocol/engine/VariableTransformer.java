package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.SCVariable;
import de.tub.pes.syscir.sc_model.variables.SCArray;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import scala.Option;
import vct.col.ast.InstanceField;
import vct.col.ast.TArray;
import vct.col.ast.Type;
import vct.col.ast.Variable;
import vct.col.origin.Origin;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.util.OriGen;

/**
 * Provides the functionality to transform a SystemC variable into a COL variable.
 *
 * @param <T> IGNORED
 */
public class VariableTransformer<T> {

    /**
     * SystemC instance the transformed variables are members of.
     */
    private final SCClassInstance sc_inst;

    /**
     * COL system context.
     */
    private final COLSystem<T> col_system;

    public VariableTransformer(SCClassInstance sc_inst, COLSystem<T> col_system) {
        this.sc_inst = sc_inst;
        this.col_system = col_system;
    }

    /**
     * Returns the COL instance field that corresponds to the given SystemC variable, or, if none exists, creates a new
     * one and adds it in the COL system.
     *
     * @param sc_var SystemC variable to be transformed
     * @return COL instance field corresponding to given SystemC variable
     */
    public InstanceField<T> transform_variable_to_instance_field(SCVariable sc_var) {
        return transform_variable_to_instance_field(sc_var, "");
    }

    /**
     * Returns the COL instance field that corresponds to the given SystemC variable, or, if none exists, creates a new
     * one and adds it in the COL system. Also uses prefixing on the preferred variable name to indicate which method
     * this field originated from.
     *
     * @param sc_var SystemC variable to be transformed
     * @param fun_name Name of the function the field is used in
     * @return COL instance field corresponding to given SystemC variable
     */
    public InstanceField<T> transform_variable_to_instance_field(SCVariable sc_var, String fun_name) {
        // Try to find the corresponding field in the COL system context
        InstanceField<T> result = col_system.get_instance_field(sc_inst, sc_var);

        if (result == null) {
            // If no such field exists, create a new one
            Origin o = OriGen.create((fun_name.isEmpty()) ? sc_var.getName() : sc_var.getName() + "_" + fun_name);
            result = new InstanceField<>(get_type(sc_var), col_system.NO_FLAGS, Option.empty(), o);
            col_system.add_instance_field_mapping(sc_inst, sc_var, result);
        }

        return result;
    }

    /**
     * Returns the COL local variable that corresponds to the given SystemC variable, or, if none exists, creates a new
     * one and adds it in the COL system.
     *
     * @param sc_var SystemC variable to be transformed
     * @return COL variable corresponding to given SystemC variable
     */
    public Variable<T> transform_variable_to_variable(SCVariable sc_var) {
        // Try to find the local variable in the COL system context
        Variable<T> result = col_system.get_variable(sc_inst, sc_var);

        if (result == null) {
            // If no such variable exists, create a new one
            result = new Variable<>(get_type(sc_var), OriGen.create(sc_var.getName()));
            col_system.add_variable_mapping(sc_inst, sc_var, result);
        }

        return result;
    }

    /**
     * Helper function to extract the type of the given SystemC variable.
     *
     * @param sc_var SystemC variable of the type that should be transformed
     * @return A COL type object representing the variable's type
     */
    private Type<T> get_type(SCVariable sc_var) {
        if (sc_var instanceof SCArray sc_array) {
            if (sc_array.isArrayOfSCClassInstances()) {
                throw new UnsupportedException("Array " + sc_array + " of class type is not supported!");     // TODO: Find way to support these arrays
            }
            else {
                return new TArray<>(col_system.parse_type(sc_array.getType()), OriGen.create());
            }
        }
        else {
            return col_system.parse_type(sc_var.getType());
        }
    }
}

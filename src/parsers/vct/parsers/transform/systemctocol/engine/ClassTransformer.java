package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.*;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import scala.Option;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;
import scala.reflect.ClassTag$;
import vct.col.ast.*;
import vct.col.ast.Class;
import vct.col.ref.LazyRef;
import vct.col.ref.Ref;
import vct.parsers.transform.systemctocol.colmodel.COLClass;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.colmodel.ProcessClass;
import vct.parsers.transform.systemctocol.colmodel.StateClass;
import vct.parsers.transform.systemctocol.util.OriGen;

/**
 * Transforms a SystemC class into one or more COL classes.
 *
 * @param <T> IGNORED
 */
public class ClassTransformer<T> {

    /**
     * The COL system context.
     */
    private final COLSystem<T> col_system;

    /**
     * A list of new instance methods that were generated during the translation process.
     */
    private final java.util.List<InstanceMethod<T>> generated_instance_methods;

    public ClassTransformer(COLSystem<T> col_system) {
        this.col_system = col_system;
        this.generated_instance_methods = new java.util.ArrayList<>();
    }

    /**
     * Transforms the given process class into a COL class encoding the process.
     *
     * @param process Process class in intermediate representation
     * @return A COL class encoding the semantics of the given process class
     */
    public Class<T> create_process_class(ProcessClass process) {
        java.util.List<ClassDeclaration<T>> declarations = new java.util.ArrayList<>();

        // Transform class attributes
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), ClassTag$.MODULE$.apply(Class.class));
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, Option.empty(), OriGen.create("m"));
        declarations.add(m);
        col_system.add_class_main_ref(process, m);
        java.util.Map<SCVariable, InstanceField<T>> fields = create_fields(process.get_generating_function(), process.get_methods(),
                process.get_attributes(), process.get_generating_instance());
        declarations.addAll(fields.values());

        // Register class attributes in COL system
        col_system.set_class_instance_fields(process, fields);
        for (InstanceField<T> field : fields.values()) {
            col_system.add_containing_class(field, process);
        }

        // Transform class constructor
        declarations.add(create_constructor(process, m, fields));

        // Transform run method
        declarations.add(create_run_method(process, m));

        // Transform other methods (it is assumed that the process is the only one using its methods)   TODO: Is that assumption true?
        declarations.addAll(create_methods(process.get_methods(), process.get_generating_instance(), m, process));

        // Add all newly generated methods to the declarations as well
        declarations.addAll(generated_instance_methods);

        return new Class<>(List.from(CollectionConverters.asScala(declarations)), col_system.NO_CLS_REFS, col_system.TRUE,
                OriGen.create(create_name(process.get_generating_instance(), process.get_generating_function())));
    }

    /**
     * Encodes the given state class into a COL class that contains its functionality.
     *
     * @param state_class State class in intermediate representation
     * @return A COL class encoding the semantics of the given state class
     */
    public Class<T> create_state_class(StateClass state_class) {
        java.util.List<ClassDeclaration<T>> declarations = new java.util.ArrayList<>();

        // Transform class attributes
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), ClassTag$.MODULE$.apply(Class.class));
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, Option.empty(), OriGen.create("m"));
        declarations.add(m);
        col_system.add_class_main_ref(state_class, m);
        java.util.Map<SCVariable, InstanceField<T>> fields = create_fields(null, state_class.get_methods(),
                state_class.get_attributes(), state_class.get_generating_instance());
        declarations.addAll(fields.values());

        // Register class attributes in COL system
        col_system.set_class_instance_fields(state_class, fields);
        for (InstanceField<T> field : fields.values()) {
            col_system.add_containing_class(field, state_class);
        }

        // Transform class constructor
        declarations.add(create_constructor(state_class, m, fields));

        // Transform other methods
        for (SCFunction method : state_class.get_methods()) {

            // Transform each method once for each process that uses it
            for (ProcessClass process : col_system.get_function_usages(method)) {

                // Ignore the process if it belongs to a different instance of the same class (i.e. it has a different state class),
                // otherwise create a new method for this process
                SCClassInstance own_inst = state_class.get_generating_instance();
                SCClassInstance proc_inst = process.get_generating_instance();
                if (!own_inst.getSCClass().equals(proc_inst.getSCClass()) || (own_inst.equals(proc_inst))) {
                    declarations.add(create_method(method, own_inst, m, state_class, process));
                }
            }
        }

        // Add newly generated methods to declaration list
        declarations.addAll(generated_instance_methods);

        return new Class<>(List.from(CollectionConverters.asScala(declarations)), col_system.NO_CLS_REFS, col_system.TRUE,
                OriGen.create(create_name(state_class.get_generating_instance())));
    }

    /**
     * Collects all variables associated with the SystemC system and converts them to COL variables. Also collects
     * member function local variables and converts them to class-level attributes to allow later verification to access
     * them globally.
     *
     * @param run_method Thread function if the SystemC class contains one
     * @param methods Other methods the SystemC class contains
     * @param attributes Attributes of the SystemC class
     * @param sc_inst Generating SystemC class instance
     * @return A map from SystemC variables to their translations as fields in COL
     */
    private java.util.Map<SCVariable, InstanceField<T>> create_fields(SCFunction run_method, java.util.List<SCFunction> methods,
                                                                      java.util.List<SCVariable> attributes, SCClassInstance sc_inst) {
        java.util.Map<SCVariable, InstanceField<T>> result = new java.util.HashMap<>();

        // Create new VariableTransformer
        VariableTransformer<T> variable_transformer = new VariableTransformer<>(sc_inst, col_system);

        // Transform attribute variables
        for (SCVariable attribute : attributes) {
            result.put(attribute, variable_transformer.transform_variable_to_instance_field(attribute));
        }

        // Transform run method local variables
        if (run_method != null) {
            for (SCVariable local_var : run_method.getLocalVariables()) {
                InstanceField<T> field = variable_transformer.transform_variable_to_instance_field(local_var, run_method.getName());
                if (!result.containsKey(local_var)) {
                    result.put(local_var, field);
                }
            }
        }

        // Transform other method local variables
        for (SCFunction method : methods) {
            for (SCVariable local_var : method.getLocalVariables()) {
                InstanceField<T> field = variable_transformer.transform_variable_to_instance_field(local_var, method.getName());
                if (!result.containsKey(local_var)) {
                    result.put(local_var, field);
                }
            }
        }

        return result;
    }

    /**
     * Creates a constructor for the given intermediate representation class. Transforms the given class's constructor
     * if one exists, and generates a basic one otherwise.
     *
     * @param col_class Class containing the constructor
     * @param m Main reference field of the transformed class
     * @param fields A map from SystemC variables to the corresponding translated fields in COL
     * @return Constructor for the COL class
     */
    private PVLConstructor<T> create_constructor(COLClass col_class, InstanceField<T> m, java.util.Map<SCVariable, InstanceField<T>> fields) {
        // Generate constructor
        FunctionTransformer<T> function_transformer = new FunctionTransformer<>(col_class.get_generating_instance(), m, col_system, col_class);
        PVLConstructor<T> pvl_constructor =  function_transformer.transform_constructor(col_class, fields);

        // Handle possible newly generated methods
        generated_instance_methods.addAll(function_transformer.get_additional_methods());

        // Return the constructor
        return pvl_constructor;
    }

    /**
     * Creates a run method based on the given thread method.
     *
     * @param process Process class containing the run method
     * @param m Main reference field of the transformed class
     * @return Run method for the COL class
     */
    private RunMethod<T> create_run_method(ProcessClass process, InstanceField<T> m) {
        // Translate run method
        FunctionTransformer<T> function_transformer = new FunctionTransformer<>(process.get_generating_instance(), m, col_system, process);
        RunMethod<T> run_method = function_transformer.transform_run_method(process);

        // Handle possible newly generated methods
        generated_instance_methods.addAll(function_transformer.get_additional_methods());

        // Return the run method
        return run_method;
    }

    /**
     * Converts a given list of process class methods from SystemC to COL.
     *
     * @param methods Methods to be converted
     * @param sc_inst Generating SystemC class instance
     * @param m Main reference field of the transformed class
     * @param process Process class using these methods
     * @return A list of methods containing the converted SystemC methods and any auxiliary methods that might have been
     *         generated along the way
     */
    private java.util.List<InstanceMethod<T>> create_methods(java.util.List<SCFunction> methods, SCClassInstance sc_inst,
                                                             InstanceField<T> m, ProcessClass process) {
        java.util.List<InstanceMethod<T>> results = new java.util.ArrayList<>();

        for (SCFunction method : methods) {
            results.add(create_method(method, sc_inst, m, process, process));
        }

        return results;
    }

    /**
     * Converts a given method from SystemC to COL.
     *
     * @param method Method to be converted
     * @param sc_inst Generating SystemC class instance
     * @param m Main reference field to the transformed class
     * @param col_class Intermediate representation class that contains this method
     * @param process Process class using this method
     * @return The converted COL method
     */
    private InstanceMethod<T> create_method(SCFunction method, SCClassInstance sc_inst, InstanceField<T> m,
                                                            COLClass col_class, ProcessClass process) {
        FunctionTransformer<T> function_transformer = new FunctionTransformer<>(sc_inst, m, col_system, col_class);

        // Add the transformed method itself
        InstanceMethod<T> result = function_transformer.transform_method(method, process);

        // Add all other generated methods
        generated_instance_methods.addAll(function_transformer.get_additional_methods());

        // Register the method in the COL system context by its SystemC equivalent and generating instance
        col_system.add_method_containing_class(method, sc_inst, col_class);

        return result;
    }

    /**
     * Creates a unique name for the COL class based on the SystemC class instance and process definition the class
     * represents.
     *
     * @param sc_class_inst Generating instance for this class
     * @param thread_function Thread function encoded in this class
     * @return A class name that communicates its origin in the SystemC design and the encoded function via prefixing
     */
    private String create_name(SCClassInstance sc_class_inst, SCFunction thread_function) {
        if (thread_function == null) return create_name(sc_class_inst);
        return create_name(sc_class_inst) + "_" + thread_function.getName();
    }

    /**
     * Creates a unique name for the COL class based on the instance the class represents.
     *
     * @param sc_class_inst Generating instance of this class
     * @return A name that communicates via prefixing which class and which class instance this class is based on
     */
    private String create_name(SCClassInstance sc_class_inst) {
        String name = sc_class_inst.getSCClass().getName();
        if (sc_class_inst.getSCClass().getInstances().size() > 1) {
            name = name + "_" + sc_class_inst.getName();
        }
        name = name.substring(0, 1).toUpperCase() + name.substring(1);
        if(name.equals("Main")) name = "SCMain";
        return name;
    }
}

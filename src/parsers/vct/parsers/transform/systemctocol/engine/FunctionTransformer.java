package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.SCFunction;
import de.tub.pes.syscir.sc_model.SCParameter;
import de.tub.pes.syscir.sc_model.SCVariable;
import de.tub.pes.syscir.sc_model.expressions.Expression;
import de.tub.pes.syscir.sc_model.expressions.FunctionCallExpression;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import scala.Option;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;
import scala.reflect.ClassTag$;
import vct.col.ast.*;
import vct.col.ref.DirectRef;
import vct.col.ref.Ref;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.colmodel.COLClass;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.colmodel.ProcessClass;
import vct.parsers.transform.systemctocol.util.GeneratedBlame;
import vct.parsers.transform.systemctocol.util.OriGen;
import vct.parsers.transform.systemctocol.util.Seqs;

/**
 * Transforms given SystemC functions into either constructors, run methods or regular methods in the COL system.
 *
 * @param <T> IGNORED
 */
public class FunctionTransformer<T> {

    /**
     * SystemC class instance that the class the method is in is based on.
     */
    private final SCClassInstance sc_inst;

    /**
     * Main field of the surrounding COL class.
     */
    private final InstanceField<T> m;

    /**
     * COL system context.
     */
    private final COLSystem<T> col_system;

    /**
     * COL class that contains this function.
     */
    private final COLClass col_class;

    /**
     * A list of additional methods that were generated in the process of creating the last processed method.
     */
    private java.util.List<InstanceMethod<T>> additional_methods;

    /**
     * Indicator whether the method is pure or not.
     */
    private boolean pure;

    public FunctionTransformer(SCClassInstance sc_inst, InstanceField<T> m, COLSystem<T> col_system, COLClass col_class) {
        this.sc_inst = sc_inst;
        this.m = m;
        this.col_system = col_system;
        this.col_class = col_class;
        this.additional_methods = new java.util.ArrayList<>();
    }

    /**
     * Transforms the given SystemC function into a COL method.
     *
     * @param function SystemC function to be transformed
     * @param process Process class that is using this function
     * @return A COL instance method with the same semantics as the given function
     */
    public InstanceMethod<T> transform_method(SCFunction function, ProcessClass process) {
        // Get return type
        Type<T> return_type = col_system.parse_type(function.getReturnType());

        // Create map from SystemC parameters to new local variables (parameters)
        java.util.Map<SCVariable, Variable<T>> params = transform_parameters(function);
        List<Variable<T>> parameters = List.from(CollectionConverters.asScala(params.values()));

        // Any function is true until the ExpressionTransformer encounters a statement that makes it not pure
        pure = true;
        // Transform body
        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(transform_body(function, process, params))), OriGen.create());

        // Create contract
        ApplicableContract<T> contract = generate_method_specifications();

        // Create method
        InstanceMethod<T> new_method = new InstanceMethod<>(return_type, parameters, col_system.NO_VARS, col_system.NO_VARS,
                Option.apply(body), contract, false, pure, new GeneratedBlame<>(), OriGen.create(function.getName()));

        // Register method in COL system context and return
        col_system.add_instance_method(function, sc_inst, process, new_method);
        return new_method;
    }

    /**
     * Generates the contract for a transformed normal SystemC method.
     *
     * @return Contract with basic permissions and constraints for the transformed method
     */
    private ApplicableContract<T> generate_method_specifications() {
        if (pure) return col_system.to_applicable_contract(col_system.TRUE, col_system.TRUE);

        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        return specification_transformer.create_basic_method_contract();
    }

    /**
     * Transforms the run method of the given ProcessClass intermediate representation into a COL run method.
     *
     * @param process Process class that should contain the run method
     * @return A run method corresponding to the run method of the given class
     */
    public RunMethod<T> transform_run_method(ProcessClass process) {
        // Get run method
        SCFunction run_method = process.get_generating_function();

        // Get reference to m
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

        // Create body
        java.util.List<Statement<T>> statements = new java.util.ArrayList<>();
        statements.add(new Lock<>(m_deref, new GeneratedBlame<>(), OriGen.create()));
        statements.addAll(transform_body(run_method, process, new java.util.HashMap<>()));
        statements.add(new Unlock<>(m_deref, new GeneratedBlame<>(), OriGen.create()));
        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());

        // Create contract
        ApplicableContract<T> contract = generate_run_method_specifications();

        // Create run method and return
        return new RunMethod<>(Option.apply(body), contract, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Creates specifications for a run method, i.e. a method that does not hold the global lock upon being called.
     *
     * @return A contract for a run method
     */
    private ApplicableContract<T> generate_run_method_specifications() {
        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        return specification_transformer.create_run_method_contract();
    }

    /**
     * Transforms the constructor of the given COL class if it contains one, and generates a basic constructor that
     * simply sets the Main reference and returns permission to all attributes if the given constructor is null.
     *
     * @param col_class Class (optionally) containing the constructor to be transformed
     * @param fields A map from SystemC variables to fields of the class that should be initialized by the constructor
     * @return A COL constructor that returns permission to all class attributes and implements the given constructor's
     *         semantics, if available
     */
    public PVLConstructor<T> transform_constructor(COLClass col_class, java.util.Map<SCVariable, InstanceField<T>> fields) {
        // Create parameters
        java.util.List<Variable<T>> parameters = new java.util.ArrayList<>();

        // Create Main class parameter
        Variable<T> m_param = new Variable<>(m.t(), OriGen.create("m_param"));
        parameters.add(m_param);

        // Create body
        java.util.List<Statement<T>> statements = new java.util.ArrayList<>();

        // Add m assignment to body
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());
        Local<T> m_param_local = new Local<>(new DirectRef<>(m_param, ClassTag$.MODULE$.apply(Variable.class)), OriGen.create());
        statements.add(new Assign<>(m_deref, m_param_local, new GeneratedBlame<>(), OriGen.create()));

        // If there is a constructor given, add its parameters and statements to the parameters and body as well
        SCFunction constructor = col_class.get_constructor();
        if (constructor != null) {
            // Handle parameters
            java.util.Map<SCVariable, Variable<T>> new_params = transform_parameters(constructor);
            parameters.addAll(new_params.values());

            // Handle body
            if (col_class instanceof ProcessClass process) {
                statements.addAll(transform_body(constructor, process, new_params));
            }
            else {
                statements.addAll(transform_body(constructor, null, new_params));   // TODO: Improve process handling
            }
        }

        // Finalize body
        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());

        // Create constructor contract
        ApplicableContract<T> contract = generate_constructor_specifications(fields, m_param);
        
        return new PVLConstructor<>(contract, Seqs.empty(), List.from(CollectionConverters.asScala(parameters)), Option.apply(body),
                new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Creates the specifications for a constructor.
     *
     * @param fields A map from SystemC variables to the class's fields that the constructor should return permission to
     * @param m_param Constructor parameter for the Main field
     * @return A contract for the constructor
     */
    private ApplicableContract<T> generate_constructor_specifications(java.util.Map<SCVariable, InstanceField<T>> fields, Variable<T> m_param) {
        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        return specification_transformer.create_constructor_contract(fields, m_param);
    }

    /**
     * Transforms the parameters of the given SystemC function to COL variables and returns a map from the SystemC
     * parameters to the COL variables.
     *
     * @param function SystemC function to be transformed
     * @return Map from the function parameters in SystemC to the function parameters in COL
     */
    private java.util.Map<SCVariable, Variable<T>> transform_parameters(SCFunction function) {
        java.util.Map<SCVariable, Variable<T>> parameter_map = new java.util.HashMap<>();
        VariableTransformer<T> variable_transformer = new VariableTransformer<>(sc_inst, col_system);

        for (SCParameter param : function.getParameters()) {
            SCVariable var = param.getVar();
            if (col_system.is_valid_parameter(var)) {
                Variable<T> var_t = variable_transformer.transform_variable_to_variable(var);
                parameter_map.put(var, var_t);
            }
        }

        return parameter_map;
    }

    /**
     * Transforms the body of the SystemC function to an equivalent in COL.
     *
     * @param function SystemC function to be transformed
     * @param process Process class that is using this function
     * @return A list of statements making up the body of the function
     */
    private java.util.List<Statement<T>> transform_body(SCFunction function, ProcessClass process, java.util.Map<SCVariable, Variable<T>> local_vars) {
        java.util.List<Statement<T>> results = new java.util.ArrayList<>();
        ExpressionTransformer<T> expression_transformer = new ExpressionTransformer<>(m, col_system, col_class, process, local_vars);

        // Reset generated methods
        additional_methods = new java.util.ArrayList<>();

        // Transform each statement in the body of the function
        for (Expression expr : function.getBody()) {
            // If the process is null (in a state class constructor), function call expressions cannot be parsed
            // (except the generated sc_module(name()), which is ignored)
            if (process == null && expr instanceof FunctionCallExpression f && !f.getFunction().getName().equals("sc_module")) {
                throw new UnsupportedException("Function calls in state class constructors are not allowed!");
            }
            Statement<T> result = expression_transformer.create_statement(expr, sc_inst);
            if (result != null) {
                results.add(result);
            }
        }

        // Function is pure iff all expressions encountered are allowed in a pure function
        pure = expression_transformer.is_pure();

        // Add newly generated methods
        additional_methods.addAll(expression_transformer.get_generated_methods());

        return results;
    }

    /**
     * Returns additional methods that were generated while parsing the method's body.
     *
     * @return A list of newly generated methods
     */
    public java.util.List<InstanceMethod<T>> get_additional_methods() {
        return additional_methods;
    }
}

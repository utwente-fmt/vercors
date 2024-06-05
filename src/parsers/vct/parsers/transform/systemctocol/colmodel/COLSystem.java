package vct.parsers.transform.systemctocol.colmodel;

import de.tub.pes.syscir.engine.util.Pair;
import de.tub.pes.syscir.sc_model.SCFunction;
import de.tub.pes.syscir.sc_model.SCPort;
import de.tub.pes.syscir.sc_model.SCVariable;
import de.tub.pes.syscir.sc_model.expressions.ConstantExpression;
import de.tub.pes.syscir.sc_model.expressions.Expression;
import de.tub.pes.syscir.sc_model.expressions.SCVariableExpression;
import de.tub.pes.syscir.sc_model.variables.SCArray;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import de.tub.pes.syscir.sc_model.variables.SCEvent;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import scala.Option;
import scala.Tuple2;
import scala.Tuple3;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.collection.immutable.Set;
import scala.jdk.javaapi.CollectionConverters;
import scala.math.BigInt;
import scala.reflect.ClassTag$;
import vct.col.ast.*;
import vct.col.ast.Class;
import vct.col.ast.Void;
import vct.col.origin.ExpectedError;
import vct.col.ref.DirectRef;
import vct.col.ref.Ref;
import vct.parsers.ParseResult;
import vct.parsers.transform.systemctocol.exceptions.SystemCFormatException;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.util.GeneratedBlame;
import vct.parsers.transform.systemctocol.util.OriGen;
import vct.parsers.transform.systemctocol.util.Seqs;

import java.util.stream.Collectors;

/**
 * Helper class for the conversion process. This class contains some auxiliary functions and data to help with
 * bookkeeping during the transformation. It also acts as a temporary top-level AST node during the transformation and
 * is used to finalize the AST in the end.
 *
 * @param <T> IGNORED
 */
public class COLSystem<T> {

    // ============================================================================================================== //
    // ====================================== CONSTANTS AND SUPPORT FUNCTIONS ======================================= //
    // ============================================================================================================== //

    /** Constant empty set of field flags */
    public final Seq<FieldFlag<T>> NO_FLAGS;
    /** Constant empty list of variables */
    public final List<Variable<T>> NO_VARS;
    /** Constant empty list of expressions */
    public final List<Expr<T>> NO_EXPRS;
    /** Constant empty list of types */
    public final List<Type<T>> NO_TYPES;
    /** Constant empty list of signals clauses */
    public final List<SignalsClause<T>> NO_SIGNALS;
    /** Constant empty list of class references */
    public final List<Ref<T, Class<T>>> NO_CLS_REFS;
    /** Constant empty list of mappings from variable references to expressions */
    public final List<Tuple2<Ref<T, Variable<T>>, Expr<T>>> NO_GIVEN;
    /** Constant empty list of mappings from expressions to variable references */
    public final List<Tuple2<Expr<T>, Ref<T, Variable<T>>>> NO_YIELDS;
    /** Constant empty list of lists of expressions */
    public final List<Seq<Expr<T>>> NO_TRIGGERS;
    /** Constant empty list of statements */
    public final List<Statement<T>> NO_STMTS;

    /** Constant boolean value for true */
    public final BooleanValue<T> TRUE = new BooleanValue<>(true, OriGen.create());
    /** Constant boolean value for false */
    public final BooleanValue<T> FALSE = new BooleanValue<>(false, OriGen.create());
    /** Constant integer value for 1 */
    public final IntegerValue<T> ONE = new IntegerValue<>(BigInt.apply(1), OriGen.create());
    /** Constant integer value for 0 */
    public final IntegerValue<T> ZERO = new IntegerValue<>(BigInt.apply(0), OriGen.create());
    /** Constant integer value for -1 */
    public final IntegerValue<T> MINUS_ONE = new IntegerValue<>(BigInt.apply(-1), OriGen.create());
    /** Constant integer value for -2 */
    public final IntegerValue<T> MINUS_TWO = new IntegerValue<>(BigInt.apply(-2), OriGen.create());
    /** Constant integer value for -3 */
    public final IntegerValue<T> MINUS_THREE = new IntegerValue<>(BigInt.apply(-3), OriGen.create());
    /** Constant value for 1\2 */
    public final Expr<T> HALF = new RatDiv<>(new IntegerValue<>(BigInt.apply(1), OriGen.create()), new IntegerValue<>(BigInt.apply(2), OriGen.create()),
            new GeneratedBlame<>(), OriGen.create());
    /** Constant null value */
    public final Null<T> NULL = new Null<>(OriGen.create());
    /** Constant void literal */
    public final Void<T> VOID = new Void<>(OriGen.create());
    /** Constant this reference */
    public final AmbiguousThis<T> THIS = new AmbiguousThis<>(OriGen.create());

    /** Type object for integer type */
    public final TInt<T> T_INT = new TInt<>(OriGen.create());
    /** Type object for boolean type */
    public final TBool<T> T_BOOL = new TBool<>(OriGen.create());
    /** Type object for integer sequence type */
    public final TSeq<T> T_SEQ_INT = new TSeq<>(T_INT, OriGen.create());
    /** Type object for boolean sequence type */
    public final TSeq<T> T_SEQ_BOOL = new TSeq<>(T_BOOL, OriGen.create());
    /** Type object for void type */
    public final TVoid<T> T_VOID = new TVoid<>(OriGen.create());

    /**
     * Creates an empty statement block.
     *
     * @return Block without statements
     */
    public Block<T> get_empty_block() {
        return new Block<>(NO_STMTS, OriGen.create());
    }

    /**
     * Checks whether the given parameter variable is a valid parameter. It is a valid parameter if it is not of a type
     * that is irrelevant for the encoding, i.e. is one of the following:
     * <ul>
     *     <li><code>sc_module_name</code></li>
     * </ul>
     *
     * @param sc_var Variable to be checked
     * @return <code>false</code> if the variable is of an unwanted type, true otherwise
     */
    public boolean is_valid_parameter(SCVariable sc_var) {
        return !sc_var.getType().equals("sc_module_name");    // Module names are not translated
    }

    /**
     * Helper function that folds a list of expressions onto a single expression using stars.
     *
     * @param expressions List of expressions, in the order in which they should appear in the starred expression
     * @return A single expression with all given expressions connected by stars
     */
    public Expr<T> fold_star(java.util.List<Expr<T>> expressions) {
        return _fold_star(new java.util.ArrayList<>(expressions));
    }

    /**
     * Private helper function that calculates the result of <code>fold_star</code>. This function requires that the
     * given list is mutable and will modify it, so it should not be used with external function calls.
     *
     * @param expressions Mutable array list of expressions to be connected in the desired order
     * @return A single expression with all given expressions connected by stars
     */
    private Expr<T> _fold_star(java.util.ArrayList<Expr<T>> expressions) {
        if (expressions == null || expressions.size() == 0) return TRUE;
        if (expressions.size() == 1) return expressions.get(0);
        Expr<T> first = expressions.remove(0);
        return new Star<>(first, _fold_star(expressions), OriGen.create());
    }

    /**
     * Helper function that folds a list of expressions onto a single expression using conjunctions.
     *
     * @param expressions List of expressions to be connected
     * @return A conjunction of all given expressions
     */
    public Expr<T> fold_and(java.util.List<Expr<T>> expressions) {
        return _fold_and(new java.util.ArrayList<>(expressions));
    }

    /**
     * Private helper function that calculates the result of <code>fold_and</code>. This function requires that the
     * given list is mutable and will modify it, so it should not be used with external function calls.
     *
     * @param expressions Mutable array list of expressions to be connected
     * @return A conjunction of all given expressions
     */
    private Expr<T> _fold_and(java.util.ArrayList<Expr<T>> expressions) {
        if (expressions == null || expressions.size() == 0) return TRUE;
        if (expressions.size() == 1) return expressions.get(0);
        Expr<T> first = expressions.remove(0);
        return new And<>(first, _fold_and(expressions), OriGen.create());
    }

    /**
     * Helper function that folds a list of expressions onto a single expression using disjunctions.
     *
     * @param expressions List of expressions to be connected
     * @return A disjunction of all given expressions
     */
    public Expr<T> fold_or(java.util.List<Expr<T>> expressions) {
        return _fold_or(new java.util.ArrayList<>(expressions));
    }

    /**
     * Private helper function that calculates the result of <code>fold_or</code>. This function requires that the given
     * list is mutable and will modify it, so it should not be used with external function calls.
     *
     * @param expressions Mutable array list of expressions to be connected
     * @return A disjunction of all given expressions
     */
    private Expr<T> _fold_or(java.util.ArrayList<Expr<T>> expressions) {
        if (expressions == null || expressions.size() == 0) return FALSE;
        if (expressions.size() == 1) return expressions.get(0);
        Expr<T> first = expressions.remove(0);
        return new Or<>(first, _fold_or(expressions), OriGen.create());
    }

    /**
     * Converts the given string type representation into a COL type object.
     *
     * @param sc_type String representation of type as it would be written in a program file
     * @return Type object corresponding to the represented type in the COL AST
     */
    public Type<T> parse_type(String sc_type) {
        if (sc_type == null || sc_type.isEmpty()) throw new UnsupportedException("Trying to parse empty type!");
        if (sc_type.endsWith("[]")) return new TArray<>(parse_type(sc_type.substring(0, sc_type.length() - 2)), OriGen.create(sc_type));
        switch (sc_type) {
            case "int", "long", "long int", "short", "short int" -> { return T_INT; }       // TODO: what about sc_int<>, sc_uint<>, sc_bigint<> etc.?
            case "bool" -> { return T_BOOL; }                                               // TODO: what about sc_logic?
            case "void" -> { return T_VOID; }
        }

        // Type is not a (supported) fundamental datatype; try enums
        if (enums.contains(sc_type)) return T_INT;  // Translate enums to integers

        // Type is not a supported datatype
        throw new UnsupportedException("Type " + sc_type + " is not supported!");   // TODO: what about class types?
    }

    /**
     * Helper function that takes two expression representing a method's pre- and postcondition and combines them into
     * an ApplicableContract object, given that there are no signals, given, yields or decreases to be taken into
     * account, and that the context_everywhere is empty.
     *
     * @param pre Precondition of the method
     * @param post Postcondition of the method
     * @return A contract for the method including the pre- and postcondition
     */
    public ApplicableContract<T> to_applicable_contract(Expr<T> pre, Expr<T> post) {
        AccountedPredicate<T> requires = new UnitAccountedPredicate<>(pre, OriGen.create());
        AccountedPredicate<T> ensures = new UnitAccountedPredicate<>(post, OriGen.create());
        return new ApplicableContract<>(requires, ensures, TRUE, NO_SIGNALS, NO_VARS, NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Helper function for generating specifications for an array field. Returns a valid-array specification that
     * contains the size of the array (arrays with no size throw an exception!) and write permission to all fields of
     * the array.
     *
     * @param sc_arr Array variable in the SystemC system
     * @param field_deref Variable expression for the COL translation of the array
     * @param m_deref Variable expression for the Main object
     * @return A list of specifications for the given array
     */
    public java.util.List<Expr<T>> get_array_specifications(SCArray sc_arr, Expr<T> field_deref, Expr<T> m_deref) {
        java.util.List<Expr<T>> conds = new java.util.ArrayList<>();

        // Get the size of the array
        Expression size_expr = sc_arr.getDerivedSize();
        // TODO: Should really all arrays without size be disallowed?
        if (size_expr == null) throw new SystemCFormatException("Instance field array " + sc_arr + " declares no size!");

        // If the array is of a constant size, add the specification that it is a valid array of the given size
        if (size_expr instanceof ConstantExpression const_size) {
            try {
                long size = Long.parseLong(const_size.getValue());
                IntegerValue<T> arr_size = new IntegerValue<>(BigInt.apply(size), OriGen.create());
                conds.add(new ValidArray<>(field_deref, arr_size, OriGen.create()));
            }
            catch (NumberFormatException nfe) {
                throw new SystemCFormatException("Array size is of constant non-integer type!");
            }
        }
        // If the array's size is a parameter, use the parameter instead
        else if (size_expr instanceof SCVariableExpression var_expr && is_parameter(var_expr.getVar())) {
            // Get parameter field
            InstanceField<T> param = get_parameter(var_expr.getVar());
            Ref<T, InstanceField<T>> param_ref = new DirectRef<>(param, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> param_deref = new Deref<>(m_deref, param_ref, new GeneratedBlame<>(), OriGen.create());

            // Add specification
            conds.add(new ValidArray<>(field_deref, param_deref, OriGen.create()));
        }
        // Disallow all other array declarations for now        TODO: support arithmetics and maybe variables in array size declaration
        else throw new UnsupportedException("Array size can only be a constant or a parameter, not " + size_expr + "!");

        // Also add write permission to all fields of the array
        Any<T> any_index = new Any<>(new GeneratedBlame<>(), OriGen.create());
        ArrayLocation<T> arr_loc = new ArrayLocation<>(field_deref, any_index, new GeneratedBlame<>(), OriGen.create());
        conds.add(new Perm<>(arr_loc, new WritePerm<>(OriGen.create()), OriGen.create()));

        return conds;
    }

    // ============================================================================================================== //
    // ============================================================================================================== //
    // ============================================================================================================== //

    /**
     * A list of names of enums used in the SystemC system. All enums are transformed to integer values in the encoding.
     */
    private final java.util.List<String> enums;

    /**
     * The Main class of the COL encoding.
     */
    private Class<T> main;

    /**
     * The scheduler method in the Main class.
     */
    private InstanceMethod<T> main_method;

    /**
     * Global permission invariant.
     */
    private InstancePredicate<T> global_perms;

    /**
     * Permission invariant with
     */
    private InstancePredicate<T> update_perms;

    /**
     * Scheduler permission invariant.
     */
    private InstancePredicate<T> scheduler_perms;

    /**
     * Parameter permission invariant.
     */
    private InstancePredicate<T> parameter_perms;

    /**
     * Map from SystemC primitive channel instances to the predicates generated for them in the Main class.
     */
    private final java.util.Map<SCKnownType, InstancePredicate<T>> prim_channel_perms;

    /**
     * Field for the <code>process_state</code> sequence.
     */
    private InstanceField<T> process_state;

    /**
     * Field for the <code>event_state</code> sequence.
     */
    private InstanceField<T> event_state;

    /**
     * Field for the <code>primitive_channel_update</code> sequence.
     */
    private InstanceField<T> primitive_channel_update;

    /**
     * A list of all global declarations (e.g. classes) in the system; represents the top-level AST node during
     * conversion and is used to finish the AST afterward.
     */
    private final java.util.List<GlobalDeclaration<T>> global_declarations;

    /**
     * A map from SystemC class instances to the processes that should be generated from them.
     */
    private final java.util.Map<SCClassInstance, java.util.List<ProcessClass>> process_mapping;

    /**
     * A map from the intermediate representation of an intermediate representation class to its encoding in COL.
     */
    private final java.util.Map<COLClass, Class<T>> col_class_translation;

    /**
     * A map from SystemC class instances to the state class that is generated for them. Since each SystemC instance can
     * only generate at most one state class, this mapping is unique.
     */
    private final java.util.Map<SCClassInstance, StateClass> state_class_mapping;

    /**
     * A map from intermediate representation classes to their Main attribute m.
     */
    private final java.util.Map<COLClass, InstanceField<T>> class_main_ref;

    /**
     * A map from intermediate representation classes to a list of their non-generated attributes.
     */
    private final java.util.Map<COLClass, java.util.Map<SCVariable, InstanceField<T>>> class_instance_fields;

    /**
     * A map from SystemC functions to a list of all process classes that call the function at some point.
     */
    private final java.util.Map<SCFunction, java.util.List<ProcessClass>> function_usages;

    /**
     * A map from SystemC variables to their transformed equivalents in COL, provided the variable was transformed to
     * an instance field.
     */
    private final java.util.Map<Pair<SCClassInstance, SCVariable>, InstanceField<T>> instance_field_mappings;

    /**
     * A map from SystemC variables to their transformed equivalents in COL, provided the variable was transformed to
     * a local variable.
     */
    private final java.util.Map<Pair<SCClassInstance, SCVariable>, Variable<T>> variable_mappings;

    /**
     * A map from COL intermediate representation class objects to the instances that are generated for them in the Main
     * class.
     */
    private final java.util.Map<COLClass, InstanceField<T>> instance_mappings;

    /**
     * A map from global SystemC variables to the parameters they are transformed into in the COL system.
     */
    private final java.util.Map<SCVariable, InstanceField<T>> parameter_mappings;

    /**
     * Field encoding the automatically generated parameter for the FIFO buffer size.
     */
    private InstanceField<T> fifo_size_parameter;

    /**
     * A map from COL instance fields to the classes that contain them.
     */
    private final java.util.Map<InstanceField<T>, COLClass> field_containing_classes;

    /**
     * A map from pairs of SystemC functions and their defining class instances, serving as an encoding of the COL
     * instance method, to the classes that contain the respective COL instance method.
     */
    private final java.util.Map<Pair<SCFunction, SCClassInstance>, COLClass> method_containing_classes;

    /**
     * A map from functions in the SystemC system to translated instance methods in the COL system. Since each SystemC
     * function can be mapped to multiple methods in the COL system if it is used by multiple processes or if there are
     * multiple instances of it, this map also takes the generating instance and process class using the method into
     * account.
     */
    private final java.util.Map<Tuple3<SCFunction, SCClassInstance, ProcessClass>, InstanceMethod<T>> instance_methods;

    /**
     * A map from pairs of module instances and their ports to the connected SystemC-internal primitive channels.
     */
    private final java.util.Map<Pair<SCClassInstance, SCPort>, SCKnownType> primitive_port_connections;

    /**
     * A map from pairs of module instances and their ports to the connected user-defined channels.
     */
    private final java.util.Map<Pair<SCClassInstance, SCPort>, SCClassInstance> hierarchical_port_connections;

    /**
     * A map from primitive channel instances in the SystemC design to their respective instances in the AST.
     */
    private final java.util.Map<SCKnownType, InstanceField<T>> primitive_channels;

    /**
     * Map from primitive channel instances (and method index, as multiple methods might be needed per instance) to
     * their respective methods. This map is populated when the transformation of known types is complete.
     */
    private final java.util.Map<Pair<SCKnownType, Integer>, InstanceMethod<T>> primitive_instance_methods;

    /**
     * Map from primitive channel instances and the field index to their fields, if available.
     */
    private final java.util.Map<Pair<SCKnownType, Integer>, InstanceField<T>> primitive_instance_fields;

    /**
     * Map from Strings representing labels in the SystemC system to their corresponding COL label declarations.
     */
    private final java.util.Map<String, LabelDecl<T>> program_labels;

    /**
     * Map from primitive channel instances to a list of the respective event IDs associated with that channel. The
     * ordering of events depends on the transformed known type.
     */
    private final java.util.Map<SCKnownType, java.util.List<Integer>> channel_events;

    /**
     * Map from SystemC event variables to their corresponding event IDs in the transformed system.
     */
    private final java.util.Map<Pair<SCClassInstance, SCEvent>, Integer> shared_events;

    /**
     * Total number of generated event IDs; can be used to find the next free ID.
     */
    private int total_nr_events;

    /**
     * Total number of primitive channel instances that exist in the SystemC system.
     */
    private int total_nr_primitive_channels;

    /**
     * Constructor. Initializes the various maps and lists that store information about the system.
     */
    public COLSystem() {
        this.enums = new java.util.ArrayList<>();
        this.prim_channel_perms = new java.util.HashMap<>();
        this.primitive_channels = new java.util.HashMap<>();
        this.global_declarations = new java.util.ArrayList<>();
        this.process_mapping = new java.util.HashMap<>();
        this.col_class_translation = new java.util.HashMap<>();
        this.state_class_mapping = new java.util.HashMap<>();
        this.class_main_ref = new java.util.HashMap<>();
        this.class_instance_fields = new java.util.HashMap<>();
        this.function_usages = new java.util.HashMap<>();
        this.instance_field_mappings = new java.util.HashMap<>();
        this.variable_mappings = new java.util.HashMap<>();
        this.instance_mappings = new java.util.HashMap<>();
        this.parameter_mappings = new java.util.HashMap<>();
        this.field_containing_classes = new java.util.HashMap<>();
        this.method_containing_classes = new java.util.HashMap<>();
        this.instance_methods = new java.util.HashMap<>();
        this.primitive_port_connections = new java.util.HashMap<>();
        this.hierarchical_port_connections = new java.util.HashMap<>();
        this.primitive_instance_methods = new java.util.HashMap<>();
        this.primitive_instance_fields = new java.util.HashMap<>();
        this.program_labels = new java.util.HashMap<>();
        this.channel_events = new java.util.HashMap<>();
        this.shared_events = new java.util.HashMap<>();
        this.total_nr_events = 0;
        this.total_nr_primitive_channels = 0;

        java.util.List<FieldFlag<T>> no_flags = java.util.List.of();
        this.NO_FLAGS = Seq.from(CollectionConverters.asScala(no_flags));
        java.util.List<Variable<T>> no_vars = java.util.List.of();
        this.NO_VARS = List.from(CollectionConverters.asScala(no_vars));
        java.util.List<Expr<T>> no_exprs = java.util.List.of();
        this.NO_EXPRS = List.from(CollectionConverters.asScala(no_exprs));
        java.util.List<Type<T>> no_types = java.util.List.of();
        this.NO_TYPES = List.from(CollectionConverters.asScala(no_types));
        java.util.List<SignalsClause<T>> no_signals = java.util.List.of();
        this.NO_SIGNALS = List.from(CollectionConverters.asScala(no_signals));
        java.util.List<Ref<T, Class<T>>> no_cls_refs = java.util.List.of();
        this.NO_CLS_REFS = List.from(CollectionConverters.asScala(no_cls_refs));
        java.util.List<Tuple2<Ref<T, Variable<T>>, Expr<T>>> no_given = java.util.List.of();
        this.NO_GIVEN = List.from(CollectionConverters.asScala(no_given));
        java.util.List<Tuple2<Expr<T>, Ref<T, Variable<T>>>> no_yields = java.util.List.of();
        this.NO_YIELDS = List.from(CollectionConverters.asScala(no_yields));
        java.util.List<Seq<Expr<T>>> no_triggers = java.util.List.of();
        this.NO_TRIGGERS = List.from(CollectionConverters.asScala(no_triggers));
        java.util.List<Statement<T>> no_stmts = java.util.List.of();
        this.NO_STMTS = List.from(CollectionConverters.asScala(no_stmts));
    }

    /**
     * Takes the COL system intermediate representation and converts it to a COL ParseResult.
     *
     * @return Parse result of the COL transformation
     */
    public ParseResult<T> to_parse_result() {
        java.util.List<ExpectedError> expected_errors = java.util.List.of();
        return new ParseResult<>(List.from(CollectionConverters.asScala(global_declarations)),
                List.from(CollectionConverters.asScala(expected_errors)));
    }

    public void create_vesuv_entry() {
        java.util.List<Statement<T>> body = new java.util.ArrayList<>();

        // Create variable of Main class
        TClass<T> main_type = new TClass<>(new DirectRef<>(main, ClassTag$.MODULE$.apply(Class.class)), Seqs.empty(), OriGen.create());
        Variable<T> var = new Variable<>(main_type, OriGen.create("design"));

        // Constructor call
        PVLNew<T> new_expr = new PVLNew<>(main_type, Seqs.empty(), Seqs.empty(), NO_GIVEN, NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        Local<T> m_deref = new Local<>(new DirectRef<>(var, ClassTag$.MODULE$.apply(Variable.class)), OriGen.create());

        // main method call
        Ref<T, InstanceMethod<T>> scheduler_ref = new DirectRef<>(main_method, ClassTag$.MODULE$.apply(InstanceMethod.class));
        InvokeMethod<T> call_main = new InvokeMethod<>(m_deref, scheduler_ref, NO_EXPRS, NO_EXPRS, NO_TYPES, NO_GIVEN,
                NO_YIELDS, new GeneratedBlame<>(), OriGen.create());

        body.add(new LocalDecl<>(var, OriGen.create()));
        body.add(new Assign<>(m_deref, new_expr, new GeneratedBlame<>(), OriGen.create()));
        body.add(call_main);

        Block<T> block = new Block<>(List.from(CollectionConverters.asScala(body)), OriGen.create());

        global_declarations.add(new VeSUVMainMethod<>(Option.apply(block), new GeneratedBlame<>(), OriGen.create()));
    }

    // ===================================================== //
    // ================ GETTERS AND SETTERS ================ //
    // ===================================================== //

    /**
     * Registers an enum type from the SystemC system. Registered enum types are converted to integers in the encoding.
     *
     * @param enum_type_name Name of an enum type
     */
    public void add_enum(String enum_type_name) {
        this.enums.add(enum_type_name);
    }

    /**
     * Registers the Main class.
     *
     * @param main_cls Main class
     */
    public void set_main(Class<T> main_cls) {
        this.main = main_cls;
    }

    /**
     * Returns the Main class. This method returns null until the end of the transformation and should be accessed
     * primarily with LazyRefs.
     *
     * @return Main class of the encoded system
     */
    public Class<T> get_main() {
        return main;
    }

    /**
     * Registers the main scheduler method.
     *
     * @param main_method Scheduler method
     */
    public void set_main_method(InstanceMethod<T> main_method) {
        this.main_method = main_method;
    }

    /**
     * Registers the global permission invariant.
     *
     * @param inv Global permission invariant
     */
    public void set_global_perms(InstancePredicate<T> inv) {
        this.global_perms = inv;
    }

    /**
     * Returns the global permission invariant.
     *
     * @return Global permission invariant
     */
    public InstancePredicate<T> get_global_perms() {
        return global_perms;
    }

    /**
     * Registers the update permission invariant.
     *
     * @param inv Update permission invariant
     */
    public void set_update_perms(InstancePredicate<T> inv) {
        this.update_perms = inv;
    }

    /**
     * Returns the update permission invariant.
     *
     * @return Update permission invariant
     */
    public InstancePredicate<T> get_update_perms() {
        return update_perms;
    }

    /**
     * Registers the permission invariant for the scheduling variables.
     *
     * @param inv Scheduler permission invariant
     */
    public void set_scheduler_perms(InstancePredicate<T> inv) {
        this.scheduler_perms = inv;
    }

    /**
     * Returns the permission invariant for the scheduling variables.
     *
     * @return Scheduler permission invariant
     */
    public InstancePredicate<T> get_scheduler_perms() {
        return scheduler_perms;
    }

    /**
     * Registers the permission invariant for the system parameters.
     *
     * @param inv Parameter permission invariant
     */
    public void set_parameter_perms(InstancePredicate<T> inv) {
        this.parameter_perms = inv;
    }

    /**
     * Returns the permission invariant for the system parameters.
     *
     * @return Parameter permission invariant
     */
    public InstancePredicate<T> get_parameter_perms() {
        return parameter_perms;
    }

    /**
     * Registers the permission invariant for a given SystemC channel instance.
     *
     * @param sc_inst SystemC channel instance
     * @param inv Channel permission invariant
     */
    public void add_prim_channel_inv(SCKnownType sc_inst, InstancePredicate<T> inv) {
        this.prim_channel_perms.put(sc_inst, inv);
    }

    /**
     * Returns the permission invariant for the primitive channel that encodes the SystemC-internal channel
     * <code>sc_inst</code>.
     *
     * @param sc_inst SystemC channel instance
     * @return Permission invariant for the corresponding channel
     */
    public InstancePredicate<T> get_prim_channel_inv(SCKnownType sc_inst) {
        return this.prim_channel_perms.get(sc_inst);
    }

    /**
     * Returns all predicates with primitive channel permission invariants.
     *
     * @return A list of predicates with permission invariants for all primitive channels
     */
    public java.util.Collection<InstancePredicate<T>> get_all_prim_channel_invariants() {
        return prim_channel_perms.values();
    }

    /**
     * Registers the field for the process state sequence.
     *
     * @param new_proc_state Process state sequence field
     */
    public void set_process_state(InstanceField<T> new_proc_state) {
        this.process_state = new_proc_state;
    }

    /**
     * Returns the <code>process_state</code> sequence field.
     *
     * @return An instance field of the Main class containing the process state
     */
    public InstanceField<T> get_process_state() {
        return process_state;
    }

    /**
     * Registers the field for the event state sequence.
     *
     * @param new_event_state Event state sequence field
     */
    public void set_event_state(InstanceField<T> new_event_state) {
        this.event_state = new_event_state;
    }

    /**
     * Returns the <code>event_state</code> sequence field.
     *
     * @return An instance field of the Main class containing the event state
     */
    public InstanceField<T> get_event_state() {
        return event_state;
    }

    /**
     * Registers the field for the primitive channel update sequence.
     *
     * @param new_prim_update Primitive channel update sequence field
     */
    public void set_primitive_channel_update(InstanceField<T> new_prim_update) {
        this.primitive_channel_update = new_prim_update;
    }

    /**
     * Returns the <code>primitive_channel_update</code> sequence field.
     *
     * @return An instance field of the Main class containing the primitive channel updates
     */
    public InstanceField<T> get_primitive_channel_update() {
        return primitive_channel_update;
    }

    /**
     * Adds a global declaration, mostly classes. The global declarations are not changed until they are passed along to
     * the VerCors backend, so this contains the finished classes of the COL encoding.
     *
     * @param cls New global declaration
     */
    public void add_global_declaration(GlobalDeclaration<T> cls) {
        this.global_declarations.add(cls);
    }

    /**
     * Registers a process class for the given SystemC class instance.
     *
     * @param sc_inst SystemC class instance
     * @param proc Process class encoding a thread of <code>sc_inst</code>
     */
    public void add_process(SCClassInstance sc_inst, ProcessClass proc) {
        if (this.process_mapping.containsKey(sc_inst)) {
            this.process_mapping.get(sc_inst).add(proc);
        }
        else {
            this.process_mapping.put(sc_inst, new java.util.ArrayList<>(java.util.List.of(proc)));
        }
    }

    /**
     * Returns all processes that are registered in the system.
     *
     * @return A list of all generated processes
     */
    public java.util.List<ProcessClass> get_all_processes() {
        return process_mapping.values().stream().flatMap(java.util.List::stream).collect(Collectors.toList());
    }

    /**
     * Gets all processes that were generated for the given SystemC class instance.
     *
     * @param sc_inst SystemC class instance
     * @return A list of processes encoding the threads of <code>sc_inst</code>
     */
    public java.util.List<ProcessClass> get_processes(SCClassInstance sc_inst) {
        return process_mapping.get(sc_inst);
    }

    /**
     * Registers the finalized COL class for a given intermediate representation class.
     *
     * @param col_class Intermediate representation class
     * @param translation Finalized COL class object
     */
    public void add_col_class_translation(COLClass col_class, Class<T> translation) {
        this.col_class_translation.put(col_class, translation);
    }

    /**
     * Returns the finalized COL class for the given intermediate representation class.
     *
     * @param col_class Intermediate representation class
     * @return Finalized COL class object
     */
    public Class<T> get_col_class_translation(COLClass col_class) {
        return this.col_class_translation.get(col_class);
    }

    /**
     * Registers the state class for a given SystemC class instance.
     *
     * @param sc_inst SystemC class instance
     * @param state_class Corresponding state class
     */
    public void add_state_class(SCClassInstance sc_inst, StateClass state_class) {
        this.state_class_mapping.put(sc_inst, state_class);
    }

    /**
     * Returns the state class that is generated for a given SystemC class instance.
     *
     * @param sc_inst SystemC class instance
     * @return State class for that instance if it exists, null otherwise
     */
    public StateClass get_state_class(SCClassInstance sc_inst) {
        return state_class_mapping.get(sc_inst);
    }

    /**
     * Registers the Main reference field for the given class.
     *
     * @param col_class Intermediate representation class
     * @param m Field of that class that references the Main object
     */
    public void add_class_main_ref(COLClass col_class, InstanceField<T> m) {
        this.class_main_ref.put(col_class, m);
    }

    /**
     * Returns the Main reference field that is generated for the given COL class.
     *
     * @param col_class Intermediate representation class
     * @return COL instance field representing that class's Main reference
     */
    public InstanceField<T> get_class_main_ref(COLClass col_class) {
        return this.class_main_ref.get(col_class);
    }

    /**
     * Registers a translation map of fields that are generated for the given intermediate representation class.
     *
     * @param col_class Intermediate representation class
     * @param fields A map from SystemC variables to their respective fields in COL
     */
    public void set_class_instance_fields(COLClass col_class, java.util.Map<SCVariable, InstanceField<T>> fields) {
        this.class_instance_fields.put(col_class, fields);
    }

    /**
     * Returns the translation map of fields that are generated for a given class.
     *
     * @param col_class Intermediate representation class
     * @return A map from SystemC variables to the translated fields of the given class
     */
    public java.util.Map<SCVariable, InstanceField<T>> get_class_instance_fields(COLClass col_class) {
        return this.class_instance_fields.get(col_class);
    }

    /**
     * Registers the given process class as using the given SystemC function.
     *
     * @param function SystemC function
     * @param proc Process class
     */
    public void add_function_usage(SCFunction function, ProcessClass proc) {
        if (this.function_usages.containsKey(function)) {
            this.function_usages.get(function).add(proc);
        }
        else {
            this.function_usages.put(function, new java.util.ArrayList<>(java.util.List.of(proc)));
        }
    }

    /**
     * Returns all process classes that use the method corresponding to the given SystemC function.
     *
     * @param function SystemC function
     * @return A list of process classes that use the given function
     */
    public java.util.List<ProcessClass> get_function_usages(SCFunction function) {
        return this.function_usages.get(function);
    }

    /**
     * Registers the correspondence between a SystemC variable in a given SystemC class instance and the COL instance
     * field it is translated into.
     *
     * @param sc_inst SystemC class instance
     * @param sc_var SystemC variable
     * @param col_field COL instance field
     */
    public void add_instance_field_mapping(SCClassInstance sc_inst, SCVariable sc_var, InstanceField<T> col_field) {
        this.instance_field_mappings.put(new Pair<>(sc_inst, sc_var), col_field);
    }

    /**
     * Returns the COL instance field that corresponds to the given SystemC variable in the given SystemC class
     * instance.
     *
     * @param sc_inst SystemC class instance
     * @param sc_var SystemC variable
     * @return Corresponding COL instance field
     */
    public InstanceField<T> get_instance_field(SCClassInstance sc_inst, SCVariable sc_var) {
        return this.instance_field_mappings.get(new Pair<>(sc_inst, sc_var));
    }

    /**
     * Registers the correspondence between a SystemC variable in a given SystemC class instance and the COL local
     * variable it is translated into.
     *
     * @param sc_inst SystemC class instance
     * @param sc_var SystemC variable
     * @param col_var COL local variable
     */
    public void add_variable_mapping(SCClassInstance sc_inst, SCVariable sc_var, Variable<T> col_var) {
        this.variable_mappings.put(new Pair<>(sc_inst, sc_var), col_var);
    }

    /**
     * Returns the COL local variable that corresponds to the given SystemC variable in the given SystemC class
     * instance.
     *
     * @param sc_inst SystemC class instance
     * @param sc_var SystemC variable
     * @return Corresponding COL local variable
     */
    public Variable<T> get_variable(SCClassInstance sc_inst, SCVariable sc_var) {
        return this.variable_mappings.get(new Pair<>(sc_inst, sc_var));
    }

    /**
     * Registers the Main class field for a given COL class intermediate representation object.
     *
     * @param col_class Intermediate representation of COL class
     * @param field Corresponding instance field in the Main class
     */
    public void add_instance_mapping(COLClass col_class, InstanceField<T> field) {
        this.instance_mappings.put(col_class, field);
    }

    /**
     * Returns the Main class field that is generated for a given COL class intermediate representation object.
     *
     * @param col_class Intermediate representation of COL class
     * @return Corresponding instance field in the Main class
     */
    public InstanceField<T> get_instance_by_class(COLClass col_class) {
        return this.instance_mappings.get(col_class);
    }

    /**
     * Registers a system parameter, encoded by a global SystemC variable, in the COL system.
     *
     * @param sc_var SystemC global variable encoding the parameter
     * @param parameter Parameter in the COL system
     */
    public void add_parameter(SCVariable sc_var, InstanceField<T> parameter) {
        this.parameter_mappings.put(sc_var, parameter);
    }

    /**
     * Returns the parameter encoded by the given global SystemC variable.
     *
     * @param sc_var SystemC global variable encoding the parameter
     * @return The parameter the given variable has been transformed into
     */
    public InstanceField<T> get_parameter(SCVariable sc_var) {
        return this.parameter_mappings.get(sc_var);
    }

    /**
     * Returns all system parameters.
     *
     * @return A list of all system parameters
     */
    public java.util.List<InstanceField<T>> get_all_parameters() {
        return new java.util.ArrayList<>(parameter_mappings.values());
    }

    /**
     * Checks whether the given SystemC variable is a parameter of the system.
     *
     * @param sc_var SystemC variable
     * @return <code>true</code> if <code>sc_var</code> is a parameter, <code>false</code> otherwise
     */
    public boolean is_parameter(SCVariable sc_var) {
        return this.parameter_mappings.containsKey(sc_var);
    }

    /**
     * Registers the parameter for the size of the FIFO buffer.
     *
     * @param buffer_size FIFO size parameter
     */
    public void set_fifo_size_parameter(InstanceField<T> buffer_size) {
        this.fifo_size_parameter = buffer_size;
    }

    /**
     * Returns the parameter for the size of the FIFO buffer.
     *
     * @return FIFO size parameter
     */
    public InstanceField<T> get_fifo_size_parameter() {
        return fifo_size_parameter;
    }

    /**
     * Registers the COL class that contains an instance field.
     *
     * @param field COL instance field
     * @param col_class Containing class
     */
    public void add_containing_class(InstanceField<T> field, COLClass col_class) {
        this.field_containing_classes.put(field, col_class);
    }

    /**
     * Returns the COL class that contains a given field.
     *
     * @param field COL instance field
     * @return COL class that contains the field
     */
    public COLClass get_containing_class(InstanceField<T> field) {
        return this.field_containing_classes.get(field);
    }

    /**
     * Registers the COL class that contains all translations a SystemC function for a given SystemC class instance.
     *
     * @param function SystemC function
     * @param sc_inst SystemC class instance
     * @param col_class COL class
     */
    public void add_method_containing_class(SCFunction function, SCClassInstance sc_inst, COLClass col_class) {
        this.method_containing_classes.put(new Pair<>(function, sc_inst), col_class);
    }

    /**
     * Returns the COL class in which all translations of a given SystemC function for a given SystemC class instance
     * are contained.
     *
     * @param function SystemC function
     * @param sc_inst Generating SystemC class instance
     * @return Corresponding COL class object
     */
    public COLClass get_method_containing_class(SCFunction function, SCClassInstance sc_inst) {
        return this.method_containing_classes.get(new Pair<>(function, sc_inst));
    }

    /**
     * Registers the COL translation of a SystemC function for a given class instance and using process.
     *
     * @param sc_function SystemC function
     * @param sc_inst Defining SystemC class instance
     * @param process Process that uses the function
     * @param method Generated COL method that corresponds to the given function
     */
    public void add_instance_method(SCFunction sc_function, SCClassInstance sc_inst, ProcessClass process, InstanceMethod<T> method) {
        this.instance_methods.put(new Tuple3<>(sc_function, sc_inst, process), method);
    }

    /**
     * Returns the instance method that is the translation of function <code>sc_function</code> in instance
     * <code>sc_inst</code> for use with the process <code>process</code>.
     *
     * @param sc_function SystemC function
     * @param sc_inst SystemC class instance
     * @param process SystemC process that uses the function
     * @return Corresponding COL method
     */
    public InstanceMethod<T> get_instance_method(SCFunction sc_function, SCClassInstance sc_inst, ProcessClass process) {
        return this.instance_methods.get(new Tuple3<>(sc_function, sc_inst, process));
    }

    /**
     * Registers a SystemC-defined primitive channel as connected to a given port in a given module instance in the
     * SystemC system.
     *
     * @param module SystemC module instance
     * @param port Port of the module
     * @param channel Connected primitive channel
     */
    public void add_primitive_port_connection(SCClassInstance module, SCPort port, SCKnownType channel) {
        this.primitive_port_connections.put(new Pair<>(module, port), channel);
    }

    /**
     * Returns the SystemC-defined primitive channel that is connected to the given port in the given module instance.
     *
     * @param module SystemC module instance
     * @param port Port of the module
     * @return SystemC-defined primitive channel that is connected to the port of the given module
     */
    public SCKnownType get_primitive_port_connection(SCClassInstance module, SCPort port) {
        return this.primitive_port_connections.get(new Pair<>(module, port));
    }

    /**
     * Registers a user-defined channel to a port of the given module instance in the SystemC system.
     *
     * @param module SystemC module instance
     * @param port Port of the module
     * @param channel Connected SystemC channel
     */
    public void add_hierarchical_port_connection(SCClassInstance module, SCPort port, SCClassInstance channel) {
        this.hierarchical_port_connections.put(new Pair<>(module, port), channel);
    }

    /**
     * Returns the user-defined channel that is connected to the given module instance at the given port.
     *
     * @param module SystemC module instance
     * @param port Port of the module
     * @return Connected SystemC channel
     */
    public SCClassInstance get_hierarchical_port_connection(SCClassInstance module, SCPort port) {
        return this.hierarchical_port_connections.get(new Pair<>(module, port));
    }

    /**
     * Registers the instance field in the Main class for the given SystemC channel instance and increases the count for
     * the number of primitive channels in the system.
     *
     * @param sc_inst SystemC channel instance
     * @param main_field Main field containing the translated instance
     */
    public void add_primitive_channel(SCKnownType sc_inst, InstanceField<T> main_field) {
        this.primitive_channels.put(sc_inst, main_field);
        total_nr_primitive_channels += 1;
    }

    /**
     * Returns the instance field in the Main class of the given primitive channel.
     *
     * @param sc_inst SystemC channel instance
     * @return Instance field of the corresponding translated channel
     */
    public InstanceField<T> get_primitive_channel(SCKnownType sc_inst) {
        return this.primitive_channels.get(sc_inst);
    }

    /**
     * Registers a primitive channel method with a given SystemC channel and index.
     *
     * @param sc_inst SystemC channel instance
     * @param i Method index in its class
     * @param method Primitive channel method
     */
    public void add_primitive_instance_method(SCKnownType sc_inst, int i, InstanceMethod<T> method) {
        this.primitive_instance_methods.put(new Pair<>(sc_inst, i), method);
    }

    /**
     * Returns the primitive channel method corresponding to the given SystemC channel instance and the given index.
     *
     * @param sc_inst SystemC channel instance
     * @param i Method index in its class
     * @return Primitive channel method
     */
    public InstanceMethod<T> get_primitive_instance_method(SCKnownType sc_inst, int i) {
        return this.primitive_instance_methods.get(new Pair<>(sc_inst, i));
    }

    /**
     * Registers a field for a FIFO channel, given by its SystemC channel instance and the field index.
     *
     * @param sc_inst SystemC channel instance
     * @param index Index of the added field
     * @param field FIFO field
     */
    public void add_primitive_instance_field(SCKnownType sc_inst, int index, InstanceField<T> field) {
        this.primitive_instance_fields.put(new Pair<>(sc_inst, index), field);
    }

    /**
     * Returns the indexed field of the given SystemC channel instance's translation in COL.
     *
     * @param sc_inst SystemC channel instance
     * @param index Index of the desired field
     * @return Field of the channel's translation
     */
    public InstanceField<T> get_primitive_instance_field(SCKnownType sc_inst, int index) {
        return this.primitive_instance_fields.get(new Pair<>(sc_inst, index));
    }

    /**
     * Registers a correspondence between a string label in SystemC and a COL label object.
     *
     * @param sc_label SystemC label string
     * @param col_label COL label object
     */
    public void add_label(String sc_label, LabelDecl<T> col_label) {
        this.program_labels.put(sc_label, col_label);
    }

    /**
     * Returns the label corresponding to the SystemC label string.
     *
     * @param sc_label SystemC label string
     * @return COL label object
     */
    public LabelDecl<T> get_label(String sc_label) {
        return this.program_labels.get(sc_label);
    }

    /**
     * Registers a list of event IDs for a channel, given by its SystemC channel instance.
     *
     * @param sc_inst SystemC channel instance
     * @param events List of event IDs
     */
    public void add_channel_events(SCKnownType sc_inst, java.util.List<Integer> events) {
        this.channel_events.put(sc_inst, events);
        this.total_nr_events += events.size();
    }

    /**
     * Returns a list of all event IDs that were generated for the given channel.
     *
     * @param sc_inst SystemC channel instance
     * @return List of event IDs in the translated channel
     */
    public java.util.List<Integer> get_channel_events(SCKnownType sc_inst) {
        return this.channel_events.get(sc_inst);
    }

    /**
     * Stores the given event ID as a shared event and increments the total number of events.
     *
     * @param sc_inst Generating class instance
     * @param event_var SystemC event variable
     * @param id ID of the generated shared event in the COl system
     */
    public void add_shared_event(SCClassInstance sc_inst, SCEvent event_var, int id) {
        this.shared_events.put(new Pair<>(sc_inst, event_var), id);
        this.total_nr_events += 1;
    }

    /**
     * Returns the shared event ID encoded by the given SystemC class instance and event variable.
     *
     * @param sc_inst Generating class instance
     * @param event_var SystemC event variable
     * @return ID of the shared event that was generated from the event variable for this instance
     */
    public int get_shared_event(SCClassInstance sc_inst, SCEvent event_var) {
        return this.shared_events.get(new Pair<>(sc_inst, event_var));
    }

    /**
     * Adds a wait event to the event count, but does not store the wait event anywhere else.
     */
    public void add_wait_event() {
        this.total_nr_events += 1;
    }

    /**
     * Returns the total number of events that have been registered in the COL system so far.
     *
     * @return Current number of events
     */
    public int get_total_nr_events() {
        return total_nr_events;
    }

    /**
     * Returns the number of primitive channels in the COL system.
     *
     * @return Number of primitive channel instances in the COL system
     */
    public int get_nr_primitive_channels() {
        return total_nr_primitive_channels;
    }
}

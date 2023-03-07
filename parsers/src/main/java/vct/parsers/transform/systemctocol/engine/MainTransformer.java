package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.SCSystem;
import de.tub.pes.syscir.sc_model.SCVariable;
import de.tub.pes.syscir.sc_model.expressions.Expression;
import de.tub.pes.syscir.sc_model.expressions.SCVariableDeclarationExpression;
import de.tub.pes.syscir.sc_model.variables.SCArray;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;
import scala.math.BigInt;
import scala.reflect.ClassTag$;
import vct.col.ast.*;
import vct.col.ast.Class;
import vct.col.ref.DirectRef;
import vct.col.ref.LazyRef;
import vct.col.ref.Ref;
import vct.parsers.transform.systemctocol.exceptions.ExpressionParseException;
import vct.parsers.transform.systemctocol.colmodel.COLClass;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.colmodel.ProcessClass;
import vct.parsers.transform.systemctocol.colmodel.StateClass;
import vct.parsers.transform.systemctocol.util.Constants;
import vct.parsers.transform.systemctocol.util.GeneratedBlame;
import vct.parsers.transform.systemctocol.util.OriGen;

/**
 * Generates a Main class encoding the SystemC scheduler. The Main class contains all class instances from the SystemC
 * model, scheduling variables indicating the process and event state, permission invariants for the scheduler and all
 * primitive channels in the model, the global permission invariant which doubles as this class's lock invariant, and
 * the scheduling function <code>main</code>.
 *
 * @param <T> IGNORED
 */
public class MainTransformer<T> {

    /**
     * SystemC system that the scheduler should encode.
     */
    private final SCSystem sc_system;

    /**
     * COL system context.
     */
    private final COLSystem<T> col_system;

    /**
     * A list of Main class instance fields that hold the processes in the system.
     */
    private java.util.List<InstanceField<T>> processes;

    /**
     * A list of Main class instance fields that hold the state classes in the system.
     */
    private java.util.List<InstanceField<T>> state_classes;

    /**
     * A list of Main class instance fields that hold the SystemC-internal channels in the system.
     */
    private java.util.List<InstanceField<T>> channels;

    /**
     * A map mapping back from the Main class's instance fields to the classes they are based on, for making the
     * conversion more convenient.
     */
    private final java.util.Map<InstanceField<T>, COLClass> class_by_field;

    /**
     * A map mapping back from the Main class's instance fields to the SystemC-internal channels they are based on, for
     * making the conversion more convenient.
     */
    private final java.util.Map<InstanceField<T>, SCKnownType> channel_by_field;

    /**
     * Update permission invariant.
     */
    private InstancePredicate<T> update_permission_invariant;

    /**
     * Scheduler permission invariant.
     */
    private InstancePredicate<T> scheduler_invariant;

    /**
     * Parameter permission invariant.
     */
    private InstancePredicate<T> parameter_invariant;

    /**
     * Global permission invariant.
     */
    private InstancePredicate<T> global_invariant;

    /**
     * Constructor of the Main class.
     */
    private PVLConstructor<T> main_constructor;

    /**
     * Scheduler helper method <code>immediate_wakeup</code>.
     */
    private InstanceMethod<T> immediate_wakeup;

    /**
     * Scheduler helper method <code>reset_events_no_delta</code>.
     */
    private InstanceMethod<T> reset_events_no_delta;

    /**
     * Scheduler helper method <code>find_minimum_advance</code>.
     */
    private InstanceMethod<T> find_minimum_advance;

    /**
     * Scheduler helper method <code>wakeup_after_wait</code>.
     */
    private InstanceMethod<T> wakeup_after_wait;

    /**
     * Scheduler helper method <code>reset_all_events</code>.
     */
    private InstanceMethod<T> reset_all_events;

    /**
     * Main scheduler method.
     */
    private InstanceMethod<T> scheduler;

    public MainTransformer(SCSystem sc_system, COLSystem<T> col_system) {
        this.sc_system = sc_system;
        this.col_system = col_system;
        this.class_by_field = new java.util.HashMap<>();
        this.channel_by_field = new java.util.HashMap<>();
    }

    /**
     * Collects all information about the Main class stored in the COL system context and creates anything that is left
     * to generate (methods, fields, etc.). These are stored in the COL system to resolve any dangling lazy references.
     * Finally, the Main class is added to the COL system.
     */
    public void create_main_class() {
        // Create Main attributes
        create_instances();
        // Create invariant predicates
        create_update_invariant();
        create_scheduler_invariant();
        create_parameter_invariant();
        create_global_invariant();
        // Create the constructor
        create_main_constructor();
        // Create scheduler helper methods
        create_helper_methods();
        // Create scheduler
        create_scheduler();
        // Put it all together and add the Main class to the COL system context
        assemble_main();
    }

    /**
     * Collects or generates instance fields in the Main class for each instance that should exist in the system.
     */
    private void create_instances() {
        // Initialize the field lists
        processes = new java.util.ArrayList<>();
        state_classes = new java.util.ArrayList<>();
        channels = new java.util.ArrayList<>();

        // Iterate over all instances in the SystemC system and transform all translations for each of them
        for (SCClassInstance sc_inst : sc_system.getInstances()) {
            // If the instance is of a SystemC-internal type, the instance field has already been generated by the
            // KnownTypeTransformer and only need to be collected
            if (sc_inst instanceof SCKnownType sc_prim) {
                InstanceField<T> channel_inst = col_system.get_primitive_channel(sc_prim);
                channels.add(channel_inst);
                channel_by_field.put(channel_inst, sc_prim);
            }
            // Otherwise, the instance is a module instance and must be transformed to state and process classes
            else {
                java.util.List<ProcessClass> process_classes = col_system.get_processes(sc_inst);
                StateClass state_class = col_system.get_state_class(sc_inst);

                // Transform process classes generated for this instance, if there are any
                if (process_classes != null) {
                    for (ProcessClass process_class : process_classes) {
                        // Get field type
                        Class<T> transformed_class = col_system.get_col_class_translation(process_class);
                        Ref<T, Class<T>> ref_to_class = new DirectRef<>(transformed_class, ClassTag$.MODULE$.apply(Class.class));
                        Type<T> t = new TClass<>(ref_to_class, OriGen.create());

                        // Generate instance field
                        InstanceField<T> inst = new InstanceField<>(t, col_system.NO_FLAGS, OriGen.create(create_instance_name(process_class)));
                        col_system.add_instance_mapping(process_class, inst);
                        processes.add(inst);
                        class_by_field.put(inst, process_class);
                    }
                }

                // Transform the state class for this instance, if there is any
                if (state_class != null) {
                    // Get field type
                    Class<T> transformed_class = col_system.get_col_class_translation(state_class);
                    Ref<T, Class<T>> ref_to_class = new DirectRef<>(transformed_class, ClassTag$.MODULE$.apply(Class.class));
                    Type<T> t = new TClass<>(ref_to_class, OriGen.create());

                    // Generate instance field
                    InstanceField<T> inst = new InstanceField<>(t, col_system.NO_FLAGS, OriGen.create(create_instance_name(state_class)));
                    col_system.add_instance_mapping(state_class, inst);
                    state_classes.add(inst);
                    class_by_field.put(inst, state_class);
                }
            }
        }
    }

    /**
     * Creates an appropriate name for an instance field.
     *
     * @param col_class COL class the field is based on
     * @return A name that indicates both the original SystemC instance and the transformed COl class this field is
     *         associated with
     */
    private String create_instance_name(COLClass col_class) {
        return col_class.get_generating_instance().getName() + "_" + col_system.get_col_class_translation(col_class).o().preferredName();
    }

    /**
     * Generates the update permission invariant. This invariant contains permission to and the length of the scheduling
     * variable <code>primitive_channel_update</code>.
     */
    private void create_update_invariant() {
        // Create reference to primitive_channel_update
        InstanceField<T> prim_channel_update = col_system.get_primitive_channel_update();
        Ref<T, InstanceField<T>> update_ref = new DirectRef<>(prim_channel_update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> update_deref = new Deref<>(col_system.THIS, update_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> update_loc = new FieldLocation<>(col_system.THIS, update_ref, OriGen.create());

        // Create some auxiliary values
        Size<T> update_size = new Size<>(update_deref, OriGen.create());
        IntegerValue<T> nr_prim_channels = new IntegerValue<>(BigInt.apply(col_system.get_nr_primitive_channels()), OriGen.create());

        // Create predicate conditions
        Perm<T> update_perm = new Perm<>(update_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> update_length = new Eq<>(update_size, nr_prim_channels, OriGen.create());

        // Put it all together and register the predicate in the COL system
        java.util.List<Expr<T>> conditions = java.util.List.of(update_perm, update_length);
        update_permission_invariant = new InstancePredicate<>(col_system.NO_VARS, Option.apply(col_system.fold_star(conditions)),
                false, true, OriGen.create("update_permission_invariant"));
        col_system.set_update_perms(update_permission_invariant);
    }

    /**
     * Generates the scheduler invariant. The scheduler invariant contains permissions to and lengths of the scheduling
     * sequences and the condition that every entry in <code>process_state</code> must either be -1 or a valid index
     * for <code>event_state</code>.
     */
    private void create_scheduler_invariant() {
        // Create references to the scheduling variables
        Ref<T, InstanceField<T>> proc_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> proc_state_deref = new Deref<>(col_system.THIS, proc_state_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> proc_state_loc = new FieldLocation<>(col_system.THIS, proc_state_ref, OriGen.create());
        Ref<T, InstanceField<T>> ev_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> ev_state_deref = new Deref<>(col_system.THIS, ev_state_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> ev_state_loc = new FieldLocation<>(col_system.THIS, ev_state_ref, OriGen.create());

        // Create some auxiliary values
        Size<T> proc_size = new Size<>(proc_state_deref, OriGen.create());
        IntegerValue<T> nr_procs = new IntegerValue<>(BigInt.apply(ProcessClass.get_nr_processes()), OriGen.create());
        Size<T> ev_size = new Size<>(ev_state_deref, OriGen.create());
        IntegerValue<T> nr_events = new IntegerValue<>(BigInt.apply(col_system.get_total_nr_events()), OriGen.create());

        // Apply update permission invariant
        Ref<T, InstancePredicate<T>> ref_update_invariant = new DirectRef<>(update_permission_invariant, ClassTag$.MODULE$.apply(InstancePredicate.class));
        InstancePredicateApply<T> apply_update_perms = new InstancePredicateApply<>(col_system.THIS, ref_update_invariant,
                col_system.NO_EXPRS, new WritePerm<>(OriGen.create()), OriGen.create());

        // Create conditions
        Perm<T> perm_to_proc = new Perm<>(proc_state_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> proc_length = new Eq<>(proc_size, nr_procs, OriGen.create());
        Perm<T> perm_to_ev = new Perm<>(ev_state_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> ev_length = new Eq<>(ev_size, nr_events, OriGen.create());

        // Create forall statement variable
        Variable<T> i = new Variable<>(col_system.T_INT, OriGen.create("i"));
        Local<T> i_loc = new Local<>(new DirectRef<>(i, ClassTag$.MODULE$.apply(Variable.class)), OriGen.create());
        GreaterEq<T> i_lower = new GreaterEq<>(i_loc, col_system.ZERO, OriGen.create());
        Less<T> i_upper = new Less<>(i_loc, proc_size, OriGen.create());
        And<T> i_bounds = new And<>(i_lower, i_upper, OriGen.create());

        // Create forall body
        SeqSubscript<T> proc_i = new SeqSubscript<>(proc_state_deref, i_loc, new GeneratedBlame<>(), OriGen.create());
        InlinePattern<T> trigger = new InlinePattern<>(proc_i, 0, 0, OriGen.create());
        Eq<T> proc_ready = new Eq<>(trigger, col_system.MINUS_ONE, OriGen.create());
        LessEq<T> proc_lower = new LessEq<>(col_system.ZERO, proc_i, OriGen.create());
        Less<T> proc_upper = new Less<>(proc_i, ev_size, OriGen.create());
        And<T> proc_bounds = new And<>(proc_lower, proc_upper, OriGen.create());
        Or<T> body = new Or<>(proc_ready, proc_bounds, OriGen.create());

        // Create forall statement
        Implies<T> forall_body = new Implies<>(i_bounds, body, OriGen.create());
        List<Variable<T>> bindings = List.from(CollectionConverters.asScala(java.util.List.of(i)));
        Forall<T> forall = new Forall<>(bindings, col_system.NO_TRIGGERS, forall_body, OriGen.create());

        // Put it all together and register the invariant in the COL system context
        java.util.List<Expr<T>> conditions = java.util.List.of(apply_update_perms, perm_to_proc, proc_length, perm_to_ev, ev_length, forall);
        scheduler_invariant = new InstancePredicate<>(col_system.NO_VARS, Option.apply(col_system.fold_star(conditions)),
                false, true, OriGen.create("scheduler_invariant"));
        col_system.set_scheduler_perms(scheduler_invariant);
    }

    /**
     * Generates the parameter permission invariant. This invariant contains read permission to all system parameters
     * as well as some simple constraints on them (if given).
     */
    private void create_parameter_invariant() {
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();

        // Add permission for each parameter to the invariant
        for (InstanceField<T> parameter : col_system.get_all_parameters()) {
            Ref<T, InstanceField<T>> param_ref = new DirectRef<>(parameter, ClassTag$.MODULE$.apply(InstanceField.class));
            FieldLocation<T> param_loc = new FieldLocation<>(col_system.THIS, param_ref, OriGen.create());
            conditions.add(new Perm<>(param_loc, new ReadPerm<>(OriGen.create()), OriGen.create()));
        }

        // If the fifo size parameter is set, add its conditions to the invariant, too
        InstanceField<T> fifo_param = col_system.get_fifo_size_parameter();
        if (fifo_param != null) {
            Ref<T, InstanceField<T>> fifo_ref = new DirectRef<>(fifo_param, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> fifo_deref = new Deref<>(col_system.THIS, fifo_ref, new GeneratedBlame<>(), OriGen.create());
            FieldLocation<T> fifo_loc = new FieldLocation<>(col_system.THIS, fifo_ref, OriGen.create());
            // Permission to the parameter
            conditions.add(new Perm<>(fifo_loc, new ReadPerm<>(OriGen.create()), OriGen.create()));
            // Parameter must be positive
            conditions.add(new Greater<>(fifo_deref, col_system.ZERO, OriGen.create()));
        }

        // Put it all together and register the invariant in the COL system context
        parameter_invariant = new InstancePredicate<>(col_system.NO_VARS, Option.apply(col_system.fold_star(conditions)),
                false, true, OriGen.create("parameter_invariant"));
        col_system.set_parameter_perms(parameter_invariant);
    }

    /**
     * Generates the global permission invariant. The global permission invariant contains the scheduler invariant, all
     * primitive channel invariants and permission to every instance and every field of every instance, except for the
     * Main reference field of processes.
     */
    private void create_global_invariant() {
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();

        // Create call to scheduler invariant
        Ref<T, InstancePredicate<T>> ref_scheduler_invariant = new DirectRef<>(scheduler_invariant, ClassTag$.MODULE$.apply(InstancePredicate.class));
        conditions.add(new InstancePredicateApply<>(col_system.THIS, ref_scheduler_invariant, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create()));

        // Create call to parameter invariant
        Ref<T, InstancePredicate<T>> ref_parameter_invariant = new DirectRef<>(parameter_invariant, ClassTag$.MODULE$.apply(InstancePredicate.class));
        conditions.add(new InstancePredicateApply<>(col_system.THIS, ref_parameter_invariant, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create()));

        // Create calls to all primitive channel invariants
        for (InstanceField<T> prim_channel : channels) {
            InstancePredicate<T> prim_channel_inv = col_system.get_prim_channel_inv(channel_by_field.get(prim_channel));
            Ref<T, InstancePredicate<T>> ref_channel_invariant = new DirectRef<>(prim_channel_inv, ClassTag$.MODULE$.apply(InstancePredicate.class));
            conditions.add(new InstancePredicateApply<>(col_system.THIS, ref_channel_invariant, col_system.NO_EXPRS,
                    new WritePerm<>(OriGen.create()), OriGen.create()));
        }

        // Create permissions to every field of every process
        for (InstanceField<T> proc : processes) {
            conditions.add(create_field_permissions(proc));
        }

        // Create permissions to every field of every state class, plus permissions to their Main reference fields and
        // the condition that they are equal to this object
        for (InstanceField<T> state : state_classes) {
            conditions.add(create_field_permissions(state));
            Ref<T, InstanceField<T>> state_ref = new DirectRef<>(state, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> state_deref = new Deref<>(col_system.THIS, state_ref, new GeneratedBlame<>(), OriGen.create());
            Ref<T, InstanceField<T>> state_m_ref = new DirectRef<>(col_system.get_class_main_ref(class_by_field.get(state)),
                    ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> state_m_deref = new Deref<>(state_deref, state_m_ref, new GeneratedBlame<>(), OriGen.create());
            FieldLocation<T> state_m_loc = new FieldLocation<>(state_deref, state_m_ref, OriGen.create());
            conditions.add(new Perm<>(state_m_loc, new ReadPerm<>(OriGen.create()), OriGen.create()));
            conditions.add(new Eq<>(state_m_deref, col_system.THIS, OriGen.create()));
        }

        // Put the predicate together and register it in the COL system context
        global_invariant = new InstancePredicate<>(col_system.NO_VARS, Option.apply(col_system.fold_star(conditions)),
                false, true, OriGen.create("global_invariant"));
        col_system.set_global_perms(global_invariant);
    }

    /**
     * Generates the permissions to every field of the class that is associated with the given instance field.
     *
     * @param field Main instance field holding a class instance
     * @return An expression with permissions to every field of the given class instance
     */
    private Expr<T> create_field_permissions(InstanceField<T> field) {
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();

        // Get reference to the field instance
        Ref<T, InstanceField<T>> field_ref = new DirectRef<>(field, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> field_deref = new Deref<>(col_system.THIS, field_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> field_loc = new FieldLocation<>(col_system.THIS, field_ref, OriGen.create());

        // Add permissions to the field itself
        conditions.add(new Perm<>(field_loc, new ReadPerm<>(OriGen.create()), OriGen.create()));
        conditions.add(new Neq<>(field_deref, col_system.NULL, OriGen.create()));

        // Add permissions to each field of the field instance
        java.util.Map<SCVariable, InstanceField<T>> field_fields = col_system.get_class_instance_fields(class_by_field.get(field));
        for (java.util.Map.Entry<SCVariable, InstanceField<T>> field_entry : field_fields.entrySet()) {
            // Unpack entry
            SCVariable sc_var = field_entry.getKey();
            InstanceField<T> f_field = field_entry.getValue();

            // Get references to the field
            Ref<T, InstanceField<T>> f_field_ref = new DirectRef<>(f_field, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> f_field_deref = new Deref<>(field_deref, f_field_ref, new GeneratedBlame<>(), OriGen.create());
            FieldLocation<T> f_field_loc = new FieldLocation<>(field_deref, f_field_ref, OriGen.create());

            // If the variable is an array, try to return array specifications. Also, only include read permission in
            // the invariant for the array field itself
            if (sc_var instanceof SCArray sc_arr) {
                conditions.add(new Perm<>(f_field_loc, new ReadPerm<>(OriGen.create()), OriGen.create()));
                conditions.addAll(col_system.get_array_specifications(sc_arr, f_field_deref, col_system.THIS));
            }
            // For all other fields, include write permission
            else {
                conditions.add(new Perm<>(f_field_loc, new WritePerm<>(OriGen.create()), OriGen.create()));
            }
        }

        // Connect all individual conditions with the star operator
        return col_system.fold_star(conditions);
    }

    /**
     * Generates the constructor of the Main class.
     */
    private void create_main_constructor() {
        // Body of constructor
        java.util.List<Statement<T>> initializations = new java.util.ArrayList<>();

        // Create initializations for the scheduling variables
        initializations.add(create_process_state_initialization());
        initializations.add(create_event_state_initialization());
        initializations.add(create_primitive_channel_update_initialization());

        // Create initializations for all instance fields
        for (InstanceField<T> channel : channels) {
            initializations.add(create_field_initialization(channel));
        }
        for (InstanceField<T> state_class : state_classes) {
            initializations.add(create_field_initialization(state_class));
        }
        for (InstanceField<T> process : processes) {
            initializations.add(create_field_initialization(process));
        }

        // Add a commit statement to the end of the constructor to ensure the lock invariant
        initializations.add(new Commit<>(col_system.THIS, new GeneratedBlame<>(), OriGen.create()));

        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(initializations)), OriGen.create());

        // Create contract for constructor - it should guarantee as postcondition the context of the scheduler method
        Expr<T> ensures = create_scheduler_contract();
        ApplicableContract<T> contract = col_system.to_applicable_contract(col_system.TRUE, ensures);

        main_constructor = new PVLConstructor<>(contract, col_system.NO_VARS, Option.apply(body), new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Generates the initialization of the <code>process_state</code> scheduling variable. Every entry is set to -1.
     *
     * @return An assignment for the process state initialization
     */
    private Statement<T> create_process_state_initialization() {
        // Get reference to process state field
        InstanceField<T> process_state = col_system.get_process_state();
        Ref<T, InstanceField<T>> state_ref = new DirectRef<>(process_state, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> state_deref = new Deref<>(col_system.THIS, state_ref, new GeneratedBlame<>(), OriGen.create());

        // Construct the literal sequence it should be initialized as ([-1] * #processes)
        java.util.List<Expr<T>> literal_values = new java.util.ArrayList<>();
        for (int i = 0; i < ProcessClass.get_nr_processes(); i++) {
            literal_values.add(col_system.MINUS_ONE);
        }
        LiteralSeq<T> literal = new LiteralSeq<>(col_system.T_INT, List.from(CollectionConverters.asScala(literal_values)), OriGen.create());

        // Assign the literal to the field
        return new Assign<>(state_deref, literal, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Generates the initialization of the <code>event_state</code> scheduling variable. Every entry is set to -3.
     *
     * @return An assignment for the event state initialization
     */
    private Statement<T> create_event_state_initialization() {
        // Get reference to the event state field
        InstanceField<T> event_state = col_system.get_event_state();
        Ref<T, InstanceField<T>> state_ref = new DirectRef<>(event_state, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> state_deref = new Deref<>(col_system.THIS, state_ref, new GeneratedBlame<>(), OriGen.create());

        // Construct the literal sequence it should be initialized as ([-3] * #events)
        java.util.List<Expr<T>> literal_values = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_total_nr_events(); i++) {
            literal_values.add(col_system.MINUS_THREE);
        }
        LiteralSeq<T> literal = new LiteralSeq<>(col_system.T_INT, List.from(CollectionConverters.asScala(literal_values)), OriGen.create());

        // Assign the literal to the field
        return new Assign<>(state_deref, literal, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Generates the initialization of the <code>primitive_channel_update</code> scheduling variable. Every entry is set
     * to <code>false</code>.
     *
     * @return An assignment for the primitive channel update initialization
     */
    private Statement<T> create_primitive_channel_update_initialization() {
        // Get reference to the primitive channel update field
        InstanceField<T> prim_channel_update = col_system.get_primitive_channel_update();
        Ref<T, InstanceField<T>> update_ref = new DirectRef<>(prim_channel_update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> update_deref = new Deref<>(col_system.THIS, update_ref, new GeneratedBlame<>(), OriGen.create());

        // Construct the literal sequence it should be initialized as ([false] * #primitive channels)
        java.util.List<Expr<T>> literal_values = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_nr_primitive_channels(); i++) {
            literal_values.add(col_system.FALSE);
        }
        LiteralSeq<T> literal = new LiteralSeq<>(col_system.T_BOOL, List.from(CollectionConverters.asScala(literal_values)), OriGen.create());

        // Assign the literal to the field
        return new Assign<>(update_deref, literal, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Creates an initialization for a field containing a class instance. Tries to parse the constructor call, if it
     * exists, or generates a simple constructor call passing only the Main reference if it doesn't.
     *
     * @param field Main class instance field that should be initialized
     * @return An assignment for this field
     */
    private Statement<T> create_field_initialization(InstanceField<T> field) {
        // The first argument is always this
        java.util.List<Expr<T>> parameters = new java.util.ArrayList<>(java.util.List.of(col_system.THIS));

        // Get reference to field
        Ref<T, InstanceField<T>> field_ref = new DirectRef<>(field, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> field_deref = new Deref<>(col_system.THIS, field_ref, new GeneratedBlame<>(), OriGen.create());

        // Find out which type of field it is
        COLClass col_class = class_by_field.get(field);

        // Get a reference to the translated class
        SCClassInstance sc_inst;
        if (col_class == null) {
            sc_inst = channel_by_field.get(field);
        }
        else {
            sc_inst = col_class.get_generating_instance();
        }

        // Get SystemC constructor call parameters
        SCVariableDeclarationExpression declaration = sc_inst.getDeclaration();
        if (declaration != null) {
            java.util.List<Expression> params = declaration.getInitialValues();
            if (params != null && params.size() > 0) {

                // The first parameter is the name - ignore it
                params.remove(0);

                ExpressionTransformer<T> expression_transformer = new ExpressionTransformer<>(null, col_system, null, null,
                        new java.util.HashMap<>());     // TODO: Very ugly with all those null pointers

                // Transform every parameter to COL
                for (Expression expression : params) {
                    try {
                        Expr<T> expr = expression_transformer.transform_simple_expression(expression);
                        if (expr == null) throw new NullPointerException();
                        parameters.add(expr);
                    } catch (NullPointerException ignored) {    // TODO: Ugh
                        throw new ExpressionParseException("Constructor argument could not be parsed!", expression);
                    }
                }
            }
        }

        // Create the new expression and return the assignment
        PVLNew<T> new_expr = new PVLNew<>(field.t(), List.from(CollectionConverters.asScala(parameters)), col_system.NO_GIVEN,
                col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        return new Assign<>(field_deref, new_expr, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Creates the scheduler helper methods and stores them in the appropriate attributes of this class.
     */
    private void create_helper_methods() {
        immediate_wakeup = create_immediate_wakeup();
        reset_events_no_delta = create_reset_events_no_delta();
        find_minimum_advance = create_find_minimum_advance();
        wakeup_after_wait = create_wakeup_after_wait();
        reset_all_events = create_reset_all_events();
    }

    /**
     * Creates the abstract helper method <code>immediate_wakeup</code>. This method sets the <code>process_state</code>
     * of all processes waiting for an event with an <code>event_state</code> of 0 (i.e. waiting for zero time) to -1
     * (i.e. ready to run).
     *
     * @return An <code>InstanceMethod</code> object encoding the method <code>immediate_wakeup</code>
     */
    private InstanceMethod<T> create_immediate_wakeup() {
        // Create references to the event and process state variables
        Ref<T, InstanceField<T>> event_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> event_state_deref = new Deref<>(col_system.THIS, event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> process_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> process_state_deref = new Deref<>(col_system.THIS, process_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> prim_update_ref = new DirectRef<>(col_system.get_primitive_channel_update(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> prim_update_deref = new Deref<>(col_system.THIS, prim_update_ref, new GeneratedBlame<>(), OriGen.create());

        // Create general permission context
        Expr<T> context = create_helper_context();

        // Create condition on event state and primitive update sequence
        Old<T> old_event_state = new Old<>(event_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> event_state_unchanged = new Eq<>(event_state_deref, old_event_state, OriGen.create());
        Old<T> old_prim_update = new Old<>(prim_update_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> prim_update_unchanged = new Eq<>(prim_update_deref, old_prim_update, OriGen.create());
        And<T> unchanged = new And<>(event_state_unchanged, prim_update_unchanged, OriGen.create());

        // Create conditions for the changing state
        java.util.List<Expr<T>> cond_met = new java.util.ArrayList<>();
        java.util.List<Expr<T>> cond_not_met = new java.util.ArrayList<>();
        for (int i = 0; i < ProcessClass.get_nr_processes(); i++) {
            // Prepare appropriate sequence accesses
            SeqSubscript<T> proc_state_i = new SeqSubscript<>(process_state_deref, new IntegerValue<>(BigInt.apply(i), OriGen.create()),
                    new GeneratedBlame<>(), OriGen.create());
            Old<T> old_proc_state_i = new Old<>(proc_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());
            SeqSubscript<T> event_state_proc_state_i = new SeqSubscript<>(event_state_deref, old_proc_state_i, new GeneratedBlame<>(), OriGen.create());
            Old<T> old_event_state_proc_state_i = new Old<>(event_state_proc_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());

            // Create left side of implication (condition met)
            GreaterEq<T> proc_waiting = new GreaterEq<>(old_proc_state_i, col_system.ZERO, OriGen.create());
            Eq<T> event_ready = new Eq<>(old_event_state_proc_state_i, col_system.ZERO, OriGen.create());
            And<T> left_side = new And<>(proc_waiting, event_ready, OriGen.create());

            // Create right side of implication (condition met)
            Eq<T> updated_proc_state = new Eq<>(proc_state_i, col_system.MINUS_ONE, OriGen.create());

            // Add implication to the list
            cond_met.add(new Implies<>(left_side, updated_proc_state, OriGen.create()));

            // Create left side of implication (condition not met)
            Not<T> neg_left_side = new Not<>(left_side, OriGen.create());

            // Create right side of implication (condition not met)
            Eq<T> proc_state_unchanged = new Eq<>(proc_state_i, old_proc_state_i, OriGen.create());

            // Add implication to the list
            cond_not_met.add(new Implies<>(neg_left_side, proc_state_unchanged, OriGen.create()));
        }

        // Connect the state-changing conditions with and operators
        Expr<T> cond_met_expression = col_system.fold_and(cond_met);
        Expr<T> cond_not_met_expression = col_system.fold_and(cond_not_met);

        // Combine the contract and return the method
        java.util.List<Expr<T>> conditions = java.util.List.of(context, unchanged, cond_met_expression, cond_not_met_expression);
        return create_abstract_method(col_system.to_applicable_contract(context, col_system.fold_star(conditions)), "immediate_wakeup");
    }

    /**
     * Creates the abstract helper method <code>reset_events_no_delta</code>. This method sets the <code>event_state</code>
     * of all events with an <code>event_state</code> of 0 (i.e. waiting for zero time) to -2 (i.e. occurred).
     *
     * @return An <code>InstanceMethod</code> object encoding the method <code>reset_events_no_delta</code>
     */
    private InstanceMethod<T> create_reset_events_no_delta() {
        // Create references to the event and process state variables
        Ref<T, InstanceField<T>> event_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> event_state_deref = new Deref<>(col_system.THIS, event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> process_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> process_state_deref = new Deref<>(col_system.THIS, process_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> prim_update_ref = new DirectRef<>(col_system.get_primitive_channel_update(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> prim_update_deref = new Deref<>(col_system.THIS, prim_update_ref, new GeneratedBlame<>(), OriGen.create());

        // Create general permission context
        Expr<T> context = create_helper_context();

        // Create condition on process state and primitive update sequence
        Old<T> old_process_state = new Old<>(process_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> process_state_unchanged = new Eq<>(process_state_deref, old_process_state, OriGen.create());
        Old<T> old_prim_update = new Old<>(prim_update_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> prim_update_unchanged = new Eq<>(prim_update_deref, old_prim_update, OriGen.create());
        And<T> unchanged = new And<>(process_state_unchanged, prim_update_unchanged, OriGen.create());

        // Create conditions for the changing state
        java.util.List<Expr<T>> cond_met = new java.util.ArrayList<>();
        java.util.List<Expr<T>> cond_not_met = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_total_nr_events(); i++) {
            // Prepare appropriate sequence accesses
            SeqSubscript<T> ev_state_i = new SeqSubscript<>(event_state_deref, new IntegerValue<>(BigInt.apply(i), OriGen.create()),
                    new GeneratedBlame<>(), OriGen.create());
            Old<T> old_ev_state_i = new Old<>(ev_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());

            // Create left side of implication (condition met)
            Eq<T> event_ready = new Eq<>(old_ev_state_i, col_system.ZERO, OriGen.create());

            // Create right side of implication (condition met)
            Eq<T> updated_ev_state = new Eq<>(ev_state_i, col_system.MINUS_TWO, OriGen.create());

            // Add implication to the list
            cond_met.add(new Implies<>(event_ready, updated_ev_state, OriGen.create()));

            // Create left side of implication (condition not met)
            Neq<T> event_not_ready = new Neq<>(old_ev_state_i, col_system.ZERO, OriGen.create());

            // Create right side of implication (condition not met)
            Eq<T> ev_state_unchanged = new Eq<>(ev_state_i, old_ev_state_i, OriGen.create());

            // Add implication to the list
            cond_not_met.add(new Implies<>(event_not_ready, ev_state_unchanged, OriGen.create()));
        }

        // Connect the state-changing conditions with and operators
        Expr<T> cond_met_expression = col_system.fold_and(cond_met);
        Expr<T> cond_not_met_expression = col_system.fold_and(cond_not_met);

        // Combine the contract and return the method
        java.util.List<Expr<T>> conditions = java.util.List.of(context, unchanged, cond_met_expression, cond_not_met_expression);
        return create_abstract_method(col_system.to_applicable_contract(context, col_system.fold_star(conditions)), "reset_events_no_delta");
    }

    /**
     * Creates the abstract pure function <code>find_minimum_advance</code>. This function takes a sequence of integers
     * as a parameter and returns the lowest element of that sequence that is at least -1.
     *
     * @return An <code>InstanceMethod</code> object encoding the function <code>find_minimum_advance</code>
     */
    private InstanceMethod<T> create_find_minimum_advance() {
        // Create parameters
        Variable<T> vals = new Variable<>(col_system.T_SEQ_INT, OriGen.create("vals"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(vals)));

        // Create appropriate references to the parameter and the method result
        Ref<T, Variable<T>> vals_ref = new DirectRef<>(vals, ClassTag$.MODULE$.apply(Variable.class));
        Local<T> vals_local = new Local<>(vals_ref, OriGen.create());
        Ref<T, ContractApplicable<T>> this_method = new LazyRef<>(this::get_find_minimum_advance, Option.empty(),
                ClassTag$.MODULE$.apply(ContractApplicable.class));
        Result<T> result = new Result<>(this_method, OriGen.create());

        // Create precondition
        Size<T> vals_size = new Size<>(vals_local, OriGen.create());
        IntegerValue<T> size_value = new IntegerValue<>(BigInt.apply(col_system.get_total_nr_events()), OriGen.create());
        Eq<T> requires = new Eq<>(vals_size, size_value, OriGen.create());

        // Start collecting postconditions
        java.util.List<Expr<T>> ensures = new java.util.ArrayList<>();

        // Prepare conditions that depend on each event
        java.util.List<Expr<T>> lower_bound = new java.util.ArrayList<>();
        java.util.List<Expr<T>> none_exists = new java.util.ArrayList<>();
        java.util.List<Expr<T>> result_equals_timeout_left = new java.util.ArrayList<>();
        java.util.List<Expr<T>> result_equals_timeout_right = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_total_nr_events(); i++) {
            // Prepare sequence access
            SeqSubscript<T> vals_i = new SeqSubscript<>(vals_local, new IntegerValue<>(BigInt.apply(i), OriGen.create()),
                    new GeneratedBlame<>(), OriGen.create());

            // Create condition that result is a lower bound
            LessEq<T> bound = new LessEq<>(result, vals_i, OriGen.create());
            Less<T> except = new Less<>(vals_i, col_system.MINUS_ONE, OriGen.create());
            lower_bound.add(new Or<>(except, bound, OriGen.create()));

            // Create condition that no active timeout exists
            none_exists.add(except);

            // Create condition that result equals one of the active timeouts
            GreaterEq<T> active = new GreaterEq<>(vals_i, col_system.MINUS_ONE, OriGen.create());
            result_equals_timeout_left.add(active);

            // Create right side of implication
            Eq<T> equal = new Eq<>(result, vals_i, OriGen.create());
            result_equals_timeout_right.add(new And<>(active, equal, OriGen.create()));
        }

        // First postcondition: Result is a lower bound of all active event timeouts
        ensures.add(col_system.fold_and(lower_bound));

        // Second postcondition: If an active timeout exists, then result is equal to one of them, else it is 0
        Expr<T> first_impl_left = col_system.fold_and(none_exists);
        Eq<T> first_impl_right = new Eq<>(result, col_system.ZERO, OriGen.create());
        Implies<T> first_implies = new Implies<>(first_impl_left, first_impl_right, OriGen.create());
        Expr<T> second_impl_left = col_system.fold_or(result_equals_timeout_left);
        Expr<T> second_impl_right = col_system.fold_or(result_equals_timeout_right);
        Implies<T> second_implies = new Implies<>(second_impl_left, second_impl_right, OriGen.create());
        ensures.add(new And<>(first_implies, second_implies, OriGen.create()));

        // Generate contract and method and return
        ApplicableContract<T> contract = col_system.to_applicable_contract(requires, col_system.fold_star(ensures));
        return new InstanceMethod<>(col_system.T_INT, params, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract,
                false, true, new GeneratedBlame<>(), OriGen.create("find_minimum_advance"));
    }

    /**
     * Creates the abstract helper method <code>wakeup_after_wait</code>. This method sets the <code>process_state</code>
     * of all processes that are waiting on an event with an <code>event_state</code> of 0 or -1 (i.e. waiting for zero
     * time, having just occurred, or waiting for delta delays) to -1 (i.e. ready to run).
     *
     * @return An <code>InstanceMethod</code> object encoding the method <code>wakeup_after_wait</code>
     */
    private InstanceMethod<T> create_wakeup_after_wait() {
        // Create references to the event and process state variables
        Ref<T, InstanceField<T>> event_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> event_state_deref = new Deref<>(col_system.THIS, event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> process_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> process_state_deref = new Deref<>(col_system.THIS, process_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> prim_update_ref = new DirectRef<>(col_system.get_primitive_channel_update(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> prim_update_deref = new Deref<>(col_system.THIS, prim_update_ref, new GeneratedBlame<>(), OriGen.create());

        // Create general permission context
        Expr<T> context = create_helper_context();

        // Create condition on event state and primitive update sequence
        Old<T> old_event_state = new Old<>(event_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> event_state_unchanged = new Eq<>(event_state_deref, old_event_state, OriGen.create());
        Old<T> old_prim_update = new Old<>(prim_update_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> prim_update_unchanged = new Eq<>(prim_update_deref, old_prim_update, OriGen.create());
        And<T> unchanged = new And<>(event_state_unchanged, prim_update_unchanged, OriGen.create());

        // Create conditions for the changing state
        java.util.List<Expr<T>> cond_met = new java.util.ArrayList<>();
        java.util.List<Expr<T>> cond_not_met = new java.util.ArrayList<>();
        for (int i = 0; i < ProcessClass.get_nr_processes(); i++) {
            // Prepare appropriate sequence accesses
            SeqSubscript<T> proc_state_i = new SeqSubscript<>(process_state_deref, new IntegerValue<>(BigInt.apply(i), OriGen.create()),
                    new GeneratedBlame<>(), OriGen.create());
            Old<T> old_proc_state_i = new Old<>(proc_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());
            SeqSubscript<T> event_state_proc_state_i = new SeqSubscript<>(event_state_deref, old_proc_state_i, new GeneratedBlame<>(), OriGen.create());
            Old<T> old_event_state_proc_state_i = new Old<>(event_state_proc_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());

            // Create left side of implication (condition met)
            GreaterEq<T> proc_waiting = new GreaterEq<>(old_proc_state_i, col_system.ZERO, OriGen.create());
            Eq<T> event_zero = new Eq<>(old_event_state_proc_state_i, col_system.ZERO, OriGen.create());
            Eq<T> event_minus_one = new Eq<>(old_event_state_proc_state_i, col_system.MINUS_ONE, OriGen.create());
            Or<T> event_ready = new Or<>(event_zero, event_minus_one, OriGen.create());
            And<T> left_side = new And<>(proc_waiting, event_ready, OriGen.create());

            // Create right side of implication (condition met)
            Eq<T> updated_proc_state = new Eq<>(proc_state_i, col_system.MINUS_ONE, OriGen.create());

            // Add implication to the list
            cond_met.add(new Implies<>(left_side, updated_proc_state, OriGen.create()));

            // Create left side of implication (condition not met)
            Not<T> neg_left_side = new Not<>(left_side, OriGen.create());

            // Create right side of implication (condition not met)
            Eq<T> proc_state_unchanged = new Eq<>(proc_state_i, old_proc_state_i, OriGen.create());

            // Add implication to the list
            cond_not_met.add(new Implies<>(neg_left_side, proc_state_unchanged, OriGen.create()));
        }

        // Connect the state-changing conditions with and operators
        Expr<T> cond_met_expression = col_system.fold_and(cond_met);
        Expr<T> cond_not_met_expression = col_system.fold_and(cond_not_met);

        // Combine the contract and return the method
        java.util.List<Expr<T>> conditions = java.util.List.of(context, unchanged, cond_met_expression, cond_not_met_expression);
        return create_abstract_method(col_system.to_applicable_contract(context, col_system.fold_star(conditions)), "wakeup_after_wait");
    }

    /**
     * Creates the abstract helper method <code>reset_all_events</code>. This method sets the <code>event_state</code>
     * of all events with an <code>event_state</code> of 0 or -1 (i.e. waiting for zero time, having just been notified,
     * or waiting for delta delays) to -2 (i.e. occurred).
     *
     * @return An <code>InstanceMethod</code> object encoding the method <code>reset_all_events</code>
     */
    private InstanceMethod<T> create_reset_all_events() {
        // Create references to the event and process state variables
        Ref<T, InstanceField<T>> event_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> event_state_deref = new Deref<>(col_system.THIS, event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> process_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> process_state_deref = new Deref<>(col_system.THIS, process_state_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> prim_update_ref = new DirectRef<>(col_system.get_primitive_channel_update(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> prim_update_deref = new Deref<>(col_system.THIS, prim_update_ref, new GeneratedBlame<>(), OriGen.create());

        // Create general permission context
        Expr<T> context = create_helper_context();

        // Create condition on process state and primitive update sequence
        Old<T> old_process_state = new Old<>(process_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> process_state_unchanged = new Eq<>(process_state_deref, old_process_state, OriGen.create());
        Old<T> old_prim_update = new Old<>(prim_update_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Eq<T> prim_update_unchanged = new Eq<>(prim_update_deref, old_prim_update, OriGen.create());
        And<T> unchanged = new And<>(process_state_unchanged, prim_update_unchanged, OriGen.create());

        // Create conditions for the changing state
        java.util.List<Expr<T>> cond_met = new java.util.ArrayList<>();
        java.util.List<Expr<T>> cond_not_met = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_total_nr_events(); i++) {
            // Prepare appropriate sequence accesses
            SeqSubscript<T> ev_state_i = new SeqSubscript<>(event_state_deref, new IntegerValue<>(BigInt.apply(i), OriGen.create()),
                    new GeneratedBlame<>(), OriGen.create());
            Old<T> old_ev_state_i = new Old<>(ev_state_i, Option.empty(), new GeneratedBlame<>(), OriGen.create());

            // Create left side of implication (condition met)
            Eq<T> event_zero = new Eq<>(old_ev_state_i, col_system.ZERO, OriGen.create());
            Eq<T> event_minus_one = new Eq<>(old_ev_state_i, col_system.MINUS_ONE, OriGen.create());
            Or<T> left_side = new Or<>(event_zero, event_minus_one, OriGen.create());

            // Create right side of implication (condition met)
            Eq<T> updated_ev_state = new Eq<>(ev_state_i, col_system.MINUS_TWO, OriGen.create());

            // Add implication to the list
            cond_met.add(new Implies<>(left_side, updated_ev_state, OriGen.create()));

            // Create left side of implication (condition not met)
            Not<T> event_not_ready = new Not<>(left_side, OriGen.create());

            // Create right side of implication (condition not met)
            Eq<T> ev_state_unchanged = new Eq<>(ev_state_i, old_ev_state_i, OriGen.create());

            // Add implication to the list
            cond_not_met.add(new Implies<>(event_not_ready, ev_state_unchanged, OriGen.create()));
        }

        // Connect the state-changing conditions with and operators
        Expr<T> cond_met_expression = col_system.fold_and(cond_met);
        Expr<T> cond_not_met_expression = col_system.fold_and(cond_not_met);

        // Combine the contract and return the method
        java.util.List<Expr<T>> conditions = java.util.List.of(context, unchanged, cond_met_expression, cond_not_met_expression);
        return create_abstract_method(col_system.to_applicable_contract(context, col_system.fold_star(conditions)), "reset_all_events");
    }

    /**
     * Helper function creating the permissions invariants of the abstract scheduler helper methods.
     *
     * @return An expression encoding the helper permissions (which is <code>held(this) ** scheduler_permission_invariant()</code>
     *         in PVL syntax)
     */
    private Expr<T> create_helper_context() {
        // Create reference to scheduler permissions
        Ref<T, InstancePredicate<T>> scheduler_perms = new LazyRef<>(col_system::get_scheduler_perms, Option.empty(),
                ClassTag$.MODULE$.apply(InstancePredicate.class));

        // Create conditions of method context
        Held<T> held_this = new Held<>(col_system.THIS, OriGen.create());
        InstancePredicateApply<T> permission_inv = new InstancePredicateApply<>(col_system.THIS, scheduler_perms, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());

        // Put it all together and fold it with stars
        java.util.List<Expr<T>> conditions = java.util.List.of(held_this, permission_inv);
        return col_system.fold_star(conditions);
    }

    /**
     * Helper function, creating an abstract instance method (i.e. one without a body) with the given contract and
     * method name.
     *
     * @param contract Contract for the method
     * @param method_name Method name
     * @return An InstanceMethod object without a body, but with the given contract and name
     */
    private InstanceMethod<T> create_abstract_method(ApplicableContract<T> contract, String method_name) {
        return new InstanceMethod<>(col_system.T_VOID, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(),
                contract, false, false, new GeneratedBlame<>(), OriGen.create(method_name));
    }

    /**
     * Creates the scheduler method "main". This method forks all processes in the beginning, then runs in an infinite
     * loop with the scheduler body, and finally joins all other processes again. The method is written to the
     * <code>scheduler</code> attribute of this class.
     */
    private void create_scheduler() {
        Statement<T> body = create_scheduler_body();
        Expr<T> context = create_scheduler_contract();
        ApplicableContract<T> contract = col_system.to_applicable_contract(context, context);
        scheduler = new InstanceMethod<>(col_system.T_VOID, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS,
                Option.apply(body), contract, false, false, new GeneratedBlame<>(), OriGen.create("main"));
    }

    /**
     * Creates the body of the scheduler method.
     *
     * @return An encoding of the scheduler body in COL
     */
    private Statement<T> create_scheduler_body() {

        // Create fork/join of each process
        java.util.List<Fork<T>> forks = new java.util.ArrayList<>();
        java.util.List<Join<T>> joins = new java.util.ArrayList<>();
        for (InstanceField<T> process : processes) {
            Ref<T, InstanceField<T>> proc_ref = new DirectRef<>(process, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> proc_deref = new Deref<>(col_system.THIS, proc_ref, new GeneratedBlame<>(), OriGen.create());

            forks.add(new Fork<>(proc_deref, new GeneratedBlame<>(), OriGen.create()));
            joins.add(new Join<>(proc_deref, new GeneratedBlame<>(), OriGen.create()));
        }

        // Add forks to method body
        java.util.List<Statement<T>> body = new java.util.ArrayList<>(forks);

        // Add scheduler loop to method body
        Statement<T> loop_body = create_scheduler_loop_body();
        LoopInvariant<T> inv = new LoopInvariant<>(col_system.TRUE, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        body.add(new Loop<>(col_system.get_empty_block(), col_system.TRUE, col_system.get_empty_block(), inv, loop_body, OriGen.create()));

        // Add joins to method body
        body.addAll(joins);

        // Create block of all statements and return as method body
        return new Block<>(List.from(CollectionConverters.asScala(body)), OriGen.create());
    }

    /**
     * Creates the loop body of the inner loop of the scheduler.
     *
     * @return An encoding of the scheduler loop in COL
     */
    private Statement<T> create_scheduler_loop_body() {
        // Prepare sequence accesses
        Ref<T, InstanceField<T>> process_state_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> process_state_deref = new Deref<>(col_system.THIS, process_state_ref, new GeneratedBlame<>(), OriGen.create());

        // Lock the global lock
        Lock<T> lock_this = new Lock<>(col_system.THIS, new GeneratedBlame<>(), OriGen.create());

        // Call immediate_wakeup
        Ref<T, InstanceMethod<T>> iw_ref = new DirectRef<>(immediate_wakeup, ClassTag$.MODULE$.apply(InstanceMethod.class));
        InvokeMethod<T> call_iw = new InvokeMethod<>(col_system.THIS, iw_ref, col_system.NO_EXPRS, col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());

        // Call reset_events_no_delta
        Ref<T, InstanceMethod<T>> rend_ref = new DirectRef<>(reset_events_no_delta, ClassTag$.MODULE$.apply(InstanceMethod.class));
        InvokeMethod<T> call_rend = new InvokeMethod<>(col_system.THIS, rend_ref, col_system.NO_EXPRS, col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());

        // Branch: Only go further if no process is ready
        Statement<T> if_body = create_scheduler_loop_if_body();
        java.util.List<Expr<T>> conds = new java.util.ArrayList<>();
        for (int i = 0; i < ProcessClass.get_nr_processes(); i++) {
            IntegerValue<T> i_val = new IntegerValue<>(BigInt.apply(i), OriGen.create());
            SeqSubscript<T> proc_i = new SeqSubscript<>(process_state_deref, i_val, new GeneratedBlame<>(), OriGen.create());
            conds.add(new Neq<>(proc_i, col_system.MINUS_ONE, OriGen.create()));
        }
        java.util.List<Tuple2<Expr<T>, Statement<T>>> branches = java.util.List.of(new Tuple2<>(col_system.fold_and(conds), if_body));
        Branch<T> branch = new Branch<>(List.from(CollectionConverters.asScala(branches)), OriGen.create());

        // Unlock the global lock
        Unlock<T> unlock_this = new Unlock<>(col_system.THIS, new GeneratedBlame<>(), OriGen.create());

        // Combine statements to block and return
        java.util.List<Statement<T>> statements = java.util.List.of(lock_this, call_iw, call_rend, branch, unlock_this);
        return new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());
    }

    /**
     * Creates the scheduling for the case that no processes are ready after the initial wakeup phase.
     *
     * @return An encoding of the if condition from the scheduling loop in COL
     */
    private Statement<T> create_scheduler_loop_if_body() {
        // Prepare sequence accesses
        Ref<T, InstanceField<T>> event_state_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> event_state_deref = new Deref<>(col_system.THIS, event_state_ref, new GeneratedBlame<>(), OriGen.create());

        // Create local variable min_advance
        Variable<T> min_advance = new Variable<>(col_system.T_INT, OriGen.create("min_advance"));
        Local<T> ma_local = new Local<>(new DirectRef<>(min_advance, ClassTag$.MODULE$.apply(Variable.class)), OriGen.create());

        // Perform the update phase
        Statement<T> update_phase = create_update_phase();

        // Declare min_advance
        LocalDecl<T> declare_ma = new LocalDecl<>(min_advance, OriGen.create());

        // Assign to min_advance
        Ref<T, InstanceMethod<T>> fma_ref = new DirectRef<>(find_minimum_advance, ClassTag$.MODULE$.apply(InstanceMethod.class));
        java.util.List<Expr<T>> params = java.util.List.of(event_state_deref);
        MethodInvocation<T> fma_invoke = new MethodInvocation<>(col_system.THIS, fma_ref, List.from(CollectionConverters.asScala(params)),
                col_system.NO_EXPRS, col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        Assign<T> assign_ma = new Assign<>(ma_local, fma_invoke, new GeneratedBlame<>(), OriGen.create());

        // Set min_advance to zero if it is -1
        Eq<T> cond = new Eq<>(ma_local, col_system.MINUS_ONE, OriGen.create());
        Assign<T> reset_ma = new Assign<>(ma_local, col_system.ZERO, new GeneratedBlame<>(), OriGen.create());
        java.util.List<Tuple2<Expr<T>, Statement<T>>> branches = java.util.List.of(new Tuple2<>(cond, reset_ma));
        Branch<T> cond_reset_ma = new Branch<>(List.from(CollectionConverters.asScala(branches)), OriGen.create());

        // Advance delays in event_state
        java.util.List<Expr<T>> literal_values = new java.util.ArrayList<>();
        for (int i = 0; i < col_system.get_total_nr_events(); i++) {
            IntegerValue<T> i_val = new IntegerValue<>(BigInt.apply(i), OriGen.create());
            SeqSubscript<T> ev_i = new SeqSubscript<>(event_state_deref, i_val, new GeneratedBlame<>(), OriGen.create());
            Less<T> condition = new Less<>(ev_i, col_system.MINUS_ONE, OriGen.create());
            Minus<T> minus = new Minus<>(ev_i, ma_local, OriGen.create());
            literal_values.add(new Select<>(condition, col_system.MINUS_THREE, minus, OriGen.create()));
        }
        LiteralSeq<T> reset_literal = new LiteralSeq<>(col_system.T_INT, List.from(CollectionConverters.asScala(literal_values)), OriGen.create());
        Assign<T> advance_delays = new Assign<>(event_state_deref, reset_literal, new GeneratedBlame<>(), OriGen.create());

        // Call wakeup_after_wait
        Ref<T, InstanceMethod<T>> waw_ref = new DirectRef<>(wakeup_after_wait, ClassTag$.MODULE$.apply(InstanceMethod.class));
        InvokeMethod<T> call_waw = new InvokeMethod<>(col_system.THIS, waw_ref, col_system.NO_EXPRS, col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());

        // Call reset_all_events
        Ref<T, InstanceMethod<T>> rae_ref = new DirectRef<>(reset_all_events, ClassTag$.MODULE$.apply(InstanceMethod.class));
        InvokeMethod<T> call_rae = new InvokeMethod<>(col_system.THIS, rae_ref, col_system.NO_EXPRS, col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());

        // Put it all together and return
        java.util.List<Statement<T>> statements = java.util.List.of(update_phase, declare_ma, assign_ma, cond_reset_ma, advance_delays, call_waw, call_rae);
        return new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());
    }

    private Statement<T> create_update_phase() {
        java.util.List<Statement<T>> statements = new java.util.ArrayList<>();

        // For each primitive channel, add a function call to the update function
        for (SCClassInstance sc_inst : sc_system.getInstances()) {
            if (sc_inst instanceof SCKnownType channel) {       // TODO: Also support user-defined primitive channels
                // Get reference to the channel field
                InstanceField<T> channel_field = col_system.get_primitive_channel(channel);
                Ref<T, InstanceField<T>> channel_ref = new DirectRef<>(channel_field, ClassTag$.MODULE$.apply(InstanceField.class));
                Deref<T> channel_deref = new Deref<>(col_system.THIS, channel_ref, new GeneratedBlame<>(), OriGen.create());

                // Get reference to the update method
                InstanceMethod<T> update_method = col_system.get_primitive_instance_method(channel, Constants.PRIMITIVE_UPDATE_METHOD_INDEX);
                Ref<T, InstanceMethod<T>> method_ref = new DirectRef<>(update_method, ClassTag$.MODULE$.apply(InstanceMethod.class));

                // Add update function call to body
                statements.add(new InvokeMethod<>(channel_deref, method_ref, col_system.NO_EXPRS, col_system.NO_EXPRS, col_system.NO_TYPES,
                        col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create()));
            }
        }

        // Reset the primitive channel update sequence
        statements.add(create_primitive_channel_update_initialization());

        // Put it all together and return
        return new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());
    }

    /**
     * Creates the contract of the scheduler main method. This contract contains as a context predicate permissions to
     * each process in the system, half permission to each process's m attribute and the conditions that the process is
     * not null, that its m references this Main object and that the process is idle.
     *
     * @return Scheduler main method contract in COL
     */
    private Expr<T> create_scheduler_contract() {
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        for (InstanceField<T> proc : processes) {
            // Create references to the field
            Ref<T, InstanceField<T>> proc_ref = new DirectRef<>(proc, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> proc_deref = new Deref<>(col_system.THIS, proc_ref, new GeneratedBlame<>(), OriGen.create());
            FieldLocation<T> proc_loc = new FieldLocation<>(col_system.THIS, proc_ref, OriGen.create());

            // Find field's Main attribute
            InstanceField<T> m = col_system.get_class_main_ref(class_by_field.get(proc));
            Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> m_deref = new Deref<>(proc_deref, m_ref, new GeneratedBlame<>(), OriGen.create());
            FieldLocation<T> m_loc = new FieldLocation<>(proc_deref, m_ref, OriGen.create());

            // Create individual conditions
            Perm<T> proc_perm = new Perm<>(proc_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
            Neq<T> proc_not_null = new Neq<>(proc_deref, col_system.NULL, OriGen.create());
            Perm<T> m_perm = new Perm<>(m_loc, col_system.HALF, OriGen.create());
            Eq<T> m_is_this = new Eq<>(m_deref, col_system.THIS, OriGen.create());
            IdleToken<T> proc_idle = new IdleToken<>(proc_deref, OriGen.create());

            java.util.List<Expr<T>> conds = java.util.List.of(proc_perm, proc_not_null, m_perm, m_is_this, proc_idle);
            conditions.add(col_system.fold_star(conds));
        }

        conditions.add(new Committed<>(col_system.THIS, new GeneratedBlame<>(), OriGen.create()));

        return col_system.fold_star(conditions);
    }

    /**
     * Collects all instance fields, predicates and methods that have been created so far and creates the Main class
     * from them. Also adds the resulting class to the COL system.
     */
    private void assemble_main() {
        java.util.List<ClassDeclaration<T>> declarations = new java.util.ArrayList<>();

        // Add all fields to the class
        declarations.add(col_system.get_process_state());
        declarations.add(col_system.get_event_state());
        declarations.add(col_system.get_primitive_channel_update());
        declarations.addAll(processes);
        declarations.addAll(state_classes);
        declarations.addAll(channels);
        declarations.addAll(col_system.get_all_parameters());
        if (col_system.get_fifo_size_parameter() != null) declarations.add(col_system.get_fifo_size_parameter());

        // Add all instance predicates to the class
        declarations.add(update_permission_invariant);
        declarations.add(scheduler_invariant);
        declarations.add(parameter_invariant);
        declarations.addAll(col_system.get_all_prim_channel_invariants());
        declarations.add(global_invariant);

        // Add constructor to the class
        declarations.add(main_constructor);

        // Add all instance methods to the class
        declarations.add(scheduler);
        declarations.add(immediate_wakeup);
        declarations.add(reset_events_no_delta);
        declarations.add(find_minimum_advance);
        declarations.add(wakeup_after_wait);
        declarations.add(reset_all_events);

        // Create lock invariant for the Main class
        Ref<T, InstancePredicate<T>> global_invariant_ref = new DirectRef<>(global_invariant, ClassTag$.MODULE$.apply(InstancePredicate.class));
        Expr<T> lock_invariant = new InstancePredicateApply<>(col_system.THIS, global_invariant_ref, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());

        // Assemble class
        Class<T> main_class = new Class<>(List.from(CollectionConverters.asScala(declarations)), col_system.NO_CLS_REFS,
                lock_invariant, OriGen.create("Main"));

        // Register Main class in COL system context
        col_system.add_global_declaration(main_class);
        col_system.set_main(main_class);
    }

    /**
     * Returns the <code>find_minimum_advance</code> scheduler helper method. Used to create a LazyRef to this method
     * before it is finished.
     *
     * @return The <code>find_minimum_advance</code> method
     */
    public InstanceMethod<T> get_find_minimum_advance() {
        return find_minimum_advance;
    }
}

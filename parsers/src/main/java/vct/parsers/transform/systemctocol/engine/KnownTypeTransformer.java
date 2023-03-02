package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.SCClass;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.math.BigInt;
import vct.col.ast.*;
import vct.col.ast.Class;
import vct.col.origin.Origin;
import vct.col.ref.DirectRef;
import vct.col.ref.LazyRef;
import vct.col.ref.Ref;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.util.Constants;
import vct.parsers.transform.systemctocol.util.GeneratedBlame;
import vct.parsers.transform.systemctocol.util.GenericClassTag;
import vct.parsers.transform.systemctocol.util.OriGen;

/**
 * Transforms an SCKnownType (i.e. a SystemC-predefined primitive channel instance) into a COL class. We currently
 * support the following predefined channels:
 * <ul>
 *     <li> sc_fifo </li>
 *     <li> sc_signal </li>
 * </ul>
 *
 * @param <T> IGNORED
 */
public class KnownTypeTransformer<T> {

    /**
     * The SCKnownType instance that triggered the transformation.
     */
    private final SCKnownType sc_inst;

    /**
     * The COL system that is the goal of the transformation.
     */
    private final COLSystem<T> col_system;

    /**
     * Event IDs used by the generated channel.
     */
    private final java.util.List<Integer> event_ids;

    /**
     * Constructor.
     *
     * @param sc_inst SystemC primitive channel instance to be transformed
     * @param col_system COL system context
     */
    public KnownTypeTransformer(SCKnownType sc_inst, COLSystem<T> col_system) {
        this.sc_inst = sc_inst;
        this.col_system = col_system;
        this.event_ids = new java.util.ArrayList<>();
    }

    /**
     * Generates a COL class based on the triggering Known Type instance.
     */
    public void transform() {
        SCClass sc_class = sc_inst.getSCClass();
        String name = generate_name();

        // Transform the primitive channel
        Class<T> cls = switch (sc_class.getName()) {
            case "sc_fifo_int" -> transform_fifo(OriGen.create(name), col_system.T_INT);
            case "sc_fifo_bool" -> transform_fifo(OriGen.create(name), col_system.T_BOOL);
            case "sc_signal_int" -> transform_signal(OriGen.create(name), col_system.T_INT);
            case "sc_signal_bool" -> transform_signal(OriGen.create(name), col_system.T_BOOL);
            default -> throw new UnsupportedException("The known type " + sc_class.getName() + " is not supported.", sc_inst);
        };

        // Add channel class to COL system
        java.util.Collections.sort(event_ids);
        col_system.add_channel_events(sc_inst, event_ids);
        col_system.add_global_declaration(cls);

        // Add channel field to COL system
        Ref<T, Class<T>> ref_to_cls = new DirectRef<>(cls, new GenericClassTag<>());
        col_system.add_primitive_channel(sc_inst, new InstanceField<>(new TClass<>(ref_to_cls, OriGen.create()), col_system.NO_FLAGS, OriGen.create()));
    }

    /**
     * Generates an appropriate name for the COL class, with capital starting letter and prefixing if necessary.
     *
     * @return A unique name for the class
     */
    private String generate_name() {
        String name = sc_inst.getSCClass().getName();
        if (sc_inst.getSCClass().getInstances().size() > 1) {
            name = name + "_" + sc_inst.getName();
        }
        return name.substring(0, 1).toUpperCase() + name.substring(1);
    }

    /**
     * Creates an SC_FIFO_* class with the given origin and buffer data type.
     *
     * @param o Origin containing the class name
     * @param t Type of the buffer data
     * @return A class encoding the FIFO channel
     */
    private Class<T> transform_fifo(Origin o, Type<T> t) {
        // Class fields
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), new GenericClassTag<>());
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, OriGen.create("m"));
        InstanceField<T> buf = new InstanceField<>(new TSeq<>(t, OriGen.create()), col_system.NO_FLAGS, OriGen.create("buffer"));
        col_system.add_primitive_instance_field(sc_inst, buf);

        // Permission invariant for the Main class
        InstancePredicate<T> inv = create_fifo_permission_invariant(m, buf);
        col_system.add_prim_channel_inv(sc_inst, inv);

        // Class methods
        PVLConstructor<T> constructor = create_fifo_constructor(o, t, m, buf);
        InstanceMethod<T> fifo_read = create_fifo_read_method(t, m, buf);
        InstanceMethod<T> fifo_write = create_fifo_write_method(t, m, buf);

        // Register methods in COL system
        col_system.add_primitive_instance_method(sc_inst, Constants.FIFO_READ_METHOD, fifo_read);
        col_system.add_primitive_instance_method(sc_inst, Constants.FIFO_WRITE_METHOD, fifo_write);

        // Create the class
        List<ClassDeclaration<T>> seq = List.from(CollectionConverters.asScala(java.util.List.of(m, buf, constructor, fifo_read, fifo_write)));
        return new Class<>(seq, col_system.NO_CLS_REFS, col_system.TRUE, o);
    }

    /**
     * Creates the permission invariant for this FIFO queue, for use in the Main class.
     *
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @return A predicate containing permissions and constraints for the FIFO queue
     */
    private InstancePredicate<T> create_fifo_permission_invariant(InstanceField<T> m, InstanceField<T> buf) {
        // Create references to FIFO object
        Ref<T, InstanceField<T>> fifo_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(), new GenericClassTag<>());
        Deref<T> fifo_deref = new Deref<>(col_system.THIS, fifo_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> fifo_loc = new FieldLocation<>(col_system.THIS, fifo_ref, OriGen.create());

        // Create references to m attribute
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, new GenericClassTag<>());
        Deref<T> m_deref = new Deref<>(fifo_deref, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(fifo_deref, m_ref, m.o());

        // Create references to buf attribute
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(buf, new GenericClassTag<>());
        Deref<T> buf_deref = new Deref<>(fifo_deref, buf_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> buf_loc = new FieldLocation<>(fifo_deref, buf_ref, buf.o());

        // Create permissions for invariant
        Perm<T> perm_fifo = new Perm<>(fifo_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), m.o());
        Perm<T> perm_buf = new Perm<>(buf_loc, new WritePerm<>(OriGen.create()), buf.o());

        // Create functional properties for invariant
        Neq<T> fifo_not_null = new Neq<>(fifo_deref, col_system.NULL, OriGen.create());
        Eq<T> m_is_this = new Eq<>(m_deref, col_system.THIS, OriGen.create());
        LessEq<T> buf_in_bound = new LessEq<>(new Size<>(buf_deref, OriGen.create()), new IntegerValue<>(BigInt.apply(16), OriGen.create()),
                OriGen.create());  // TODO: Parameterize bound!

        // Put it all together and return
        return new InstancePredicate<>(col_system.NO_VARS,
                Option.apply(col_system.fold_star(java.util.List.of(perm_fifo, fifo_not_null, perm_m, m_is_this, perm_buf, buf_in_bound))),
                false, true, OriGen.create(generate_name().toLowerCase() + "_permission_invariant"));
    }

    /**
     * Generates the FIFO class's constructor.
     *
     * @param o Class origin
     * @param t Buffer data type
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @return FIFO constructor
     */
    private PVLConstructor<T> create_fifo_constructor(Origin o, Type<T> t, InstanceField<T> m, InstanceField<T> buf) {
        // Constructor parameter
        Variable<T> m_param = new Variable<>(m.t(), OriGen.create("m_param"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(m_param)));

        // Constructor body
        Deref<T> local_m = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Local<T> local_m_param = new Local<>(new DirectRef<>(m_param, new GenericClassTag<>()), m_param.o());
        Assign<T> m_assign = new Assign<>(local_m, local_m_param, new GeneratedBlame<>(), OriGen.create());

        Deref<T> local_buf = new Deref<>(col_system.THIS, new DirectRef<>(buf, new GenericClassTag<>()), new GeneratedBlame<>(), buf.o());
        LiteralSeq<T> empty_seq = new LiteralSeq<>(t, col_system.NO_EXPRS, buf.o());
        Assign<T> buf_assign = new Assign<>(local_buf, empty_seq, new GeneratedBlame<>(), OriGen.create());

        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(java.util.List.of(m_assign, buf_assign))), OriGen.create());

        // Constructor contract
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), m.o());
        FieldLocation<T> buf_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(buf, new GenericClassTag<>()), buf.o());

        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), m.o());
        Perm<T> perm_buf = new Perm<>(buf_loc, new WritePerm<>(OriGen.create()), buf.o());
        Eq<T> eq_m = new Eq<>(local_m, local_m_param, OriGen.create());
        Eq<T> eq_buf = new Eq<>(local_buf, empty_seq, OriGen.create());

        ApplicableContract<T> contract = new ApplicableContract<>(new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()),
                new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perm_m, eq_m, perm_buf, eq_buf)), OriGen.create()),
                col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(),
                OriGen.create());

        return new PVLConstructor<>(contract, params, Option.apply(body), new GeneratedBlame<>(), o);
    }

    /**
     * Creates the abstract read method for the FIFO queue.
     *
     * @param t Buffer data type
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @return FIFO read method
     */
    private InstanceMethod<T> create_fifo_read_method(Type<T> t, InstanceField<T> m, InstanceField<T> buf) {
        Expr<T> perms = create_general_contract(m);

        // Precondition
        Deref<T> local_buf = new Deref<>(col_system.THIS, new DirectRef<>(buf, new GenericClassTag<>()), new GeneratedBlame<>(), buf.o());
        Less<T> buf_size = new Less<>(new IntegerValue<>(BigInt.apply(0), OriGen.create()), new Size<>(local_buf, OriGen.create()), OriGen.create());

        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(new Star<>(perms, buf_size, OriGen.create()), OriGen.create());

        // Postcondition
        Ref<T, InstanceField<T>> ref_to_proc = new DirectRef<>(col_system.get_process_state(), new GenericClassTag<>());
        Ref<T, InstanceField<T>> ref_to_ev = new DirectRef<>(col_system.get_event_state(), new GenericClassTag<>());
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Deref<T> proc_deref = new Deref<>(m_deref, ref_to_proc, new GeneratedBlame<>(), col_system.get_process_state().o());
        Deref<T> ev_deref = new Deref<>(m_deref, ref_to_ev, new GeneratedBlame<>(), col_system.get_event_state().o());

        Eq<T> proc_is_old = new Eq<>(proc_deref, new Old<>(proc_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Ref<T, ContractApplicable<T>> ref = new LazyRef<>(() -> col_system.get_primitive_instance_method(sc_inst, Constants.FIFO_READ_METHOD), Option.empty(),
                new GenericClassTag<>());
        Result<T> ret = new Result<>(ref, OriGen.create());
        SeqSubscript<T> access = new SeqSubscript<>(local_buf, new IntegerValue<>(BigInt.apply(0), OriGen.create()), new GeneratedBlame<>(), OriGen.create());
        Eq<T> result = new Eq<>(ret, new Old<>(access, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Slice<T> buf_slice = new Slice<>(local_buf, new IntegerValue<>(BigInt.apply(1), OriGen.create()), new Size<>(local_buf, OriGen.create()),
                OriGen.create());
        Eq<T> res_buf = new Eq<>(local_buf, new Old<>(buf_slice, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        int next_event_id = col_system.get_total_nr_events() + Constants.FIFO_READ_EVENT;
        event_ids.add(next_event_id);
        SeqUpdate<T> ev_update = new SeqUpdate<>(ev_deref, new IntegerValue<>(BigInt.apply(next_event_id), OriGen.create()),
                new IntegerValue<>(BigInt.apply(-1), OriGen.create()), OriGen.create());
        Eq<T> ev_notify = new Eq<>(ev_deref, new Old<>(ev_update, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perms, result, proc_is_old, res_buf, ev_notify)),
                OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS,
                col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        return new InstanceMethod<>(t, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract, false, false,
                new GeneratedBlame<>(), OriGen.create("fifo_read"));
    }

    /**
     * Creates the abstract write method for the FIFO queue.
     *
     * @param t Buffer data type
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @return FIFO write method
     */
    private InstanceMethod<T> create_fifo_write_method(Type<T> t, InstanceField<T> m, InstanceField<T> buf) {
        Expr<T> perms = create_general_contract(m);
        Variable<T> new_val = new Variable<>(t, OriGen.create("new_val"));

        // Parameters
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(new_val)));

        // Precondition
        Deref<T> local_buf = new Deref<>(col_system.THIS, new DirectRef<>(buf, new GenericClassTag<>()), new GeneratedBlame<>(), buf.o());
        // TODO: Parameterize bound!
        Less<T> buf_size = new Less<>(new Size<>(local_buf, OriGen.create()), new IntegerValue<>(BigInt.apply(16), OriGen.create()), OriGen.create());

        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(new Star<>(perms, buf_size, OriGen.create()), OriGen.create());

        // Postcondition
        Ref<T, InstanceField<T>> ref_to_proc = new DirectRef<>(col_system.get_process_state(), new GenericClassTag<>());
        Ref<T, InstanceField<T>> ref_to_ev = new DirectRef<>(col_system.get_event_state(), new GenericClassTag<>());
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Deref<T> proc_deref = new Deref<>(m_deref, ref_to_proc, new GeneratedBlame<>(), col_system.get_process_state().o());
        Deref<T> ev_deref = new Deref<>(m_deref, ref_to_ev, new GeneratedBlame<>(), col_system.get_event_state().o());

        Eq<T> proc_is_old = new Eq<>(proc_deref, new Old<>(proc_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Ref<T, Variable<T>> ref_to_new_val = new DirectRef<>(new_val, new GenericClassTag<>());
        Seq<Expr<T>> literal_vals = List.from(CollectionConverters.asScala(java.util.List.of(new Local<>(ref_to_new_val, new_val.o()))));
        LiteralSeq<T> new_vals = new LiteralSeq<>(t, literal_vals, OriGen.create());
        Concat<T> concat = new Concat<>(local_buf, new_vals, OriGen.create());
        Eq<T> new_buf = new Eq<>(local_buf, new Old<>(concat, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        int next_event_id = col_system.get_total_nr_events() + Constants.FIFO_WRITE_EVENT;
        event_ids.add(next_event_id);
        SeqUpdate<T> ev_update = new SeqUpdate<>(ev_deref, new IntegerValue<>(BigInt.apply(next_event_id), OriGen.create()),
                new IntegerValue<>(BigInt.apply(-1), OriGen.create()), OriGen.create());
        Eq<T> ev_notify = new Eq<>(ev_deref, new Old<>(ev_update, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perms, proc_is_old, new_buf, ev_notify)),
                OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS,
                col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        return new InstanceMethod<>(col_system.T_VOID, params, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract, false, false,
                new GeneratedBlame<>(), OriGen.create("fifo_write"));
    }

    /**
     * Creates an SC_SIGNAL_* class with the given origin and data type.
     *
     * @param o Origin containing the class name
     * @param t Data type of the signal channel
     * @return A class encoding the signal channel
     */
    private Class<T> transform_signal(Origin o, Type<T> t) {
        // Class fields
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), new GenericClassTag<>());
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, OriGen.create("m"));
        InstanceField<T> val = new InstanceField<>(t, col_system.NO_FLAGS, OriGen.create("val"));

        // Permission invariant for the Main class
        InstancePredicate<T> inv = create_signal_permission_invariant(m, val);
        col_system.add_prim_channel_inv(sc_inst, inv);

        // Class methods
        PVLConstructor<T> constructor = create_signal_constructor(o, m, val);
        InstanceMethod<T> signal_read = create_signal_read_method(t, m, val);
        InstanceMethod<T> signal_write = create_signal_write_method(t, m, val);

        // Register methods in COL system
        col_system.add_primitive_instance_method(sc_inst, Constants.SIGNAL_READ_METHOD, signal_read);
        col_system.add_primitive_instance_method(sc_inst, Constants.SIGNAL_WRITE_METHOD, signal_write);

        // Create the class
        List<ClassDeclaration<T>> seq = List.from(CollectionConverters.asScala(java.util.List.of(m, val, constructor, signal_read, signal_write)));
        return new Class<>(seq, col_system.NO_CLS_REFS, col_system.TRUE, o);
    }

    /**
     * Creates the permission invariant for this sc_signal, for use in the Main class.
     *
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @return A predicate containing permissions and constraints for the signal instance
     */
    private InstancePredicate<T> create_signal_permission_invariant(InstanceField<T> m, InstanceField<T> val) {
        // Create references to signal
        Ref<T, InstanceField<T>> signal_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(), new GenericClassTag<>());
        Deref<T> signal_deref = new Deref<>(col_system.THIS, signal_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> signal_loc = new FieldLocation<>(col_system.THIS, signal_ref, OriGen.create());

        // Create references to m
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, new GenericClassTag<>());
        Deref<T> m_deref = new Deref<>(signal_deref, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(signal_deref, m_ref, m.o());

        // Create references to val
        Ref<T, InstanceField<T>> val_ref = new DirectRef<>(val, new GenericClassTag<>());
        FieldLocation<T> val_loc = new FieldLocation<>(signal_deref, val_ref, val.o());

        // Create permissions for invariant
        Perm<T> perm_signal = new Perm<>(signal_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), m.o());
        Perm<T> perm_val = new Perm<>(val_loc, new WritePerm<>(OriGen.create()), val.o());

        // Create functional properties for invariant
        Neq<T> signal_not_null = new Neq<>(signal_deref, col_system.NULL, OriGen.create());
        Eq<T> m_is_this = new Eq<>(m_deref, col_system.THIS, OriGen.create());

        // Put it all together and return
        return new InstancePredicate<>(col_system.NO_VARS,
                Option.apply(col_system.fold_star(java.util.List.of(perm_signal, signal_not_null, perm_m, m_is_this, perm_val))),
                false, true, OriGen.create(generate_name().toLowerCase() + "_permission_invariant"));
    }

    /**
     * Generates the signal class's constructor.
     *
     * @param o Class origin
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored value
     * @return Signal class constructor
     */
    private PVLConstructor<T> create_signal_constructor(Origin o, InstanceField<T> m, InstanceField<T> val) {
        // Constructor parameter
        Variable<T> m_param = new Variable<>(m.t(), OriGen.create("m_param"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(m_param)));

        // Constructor body
        Deref<T> local_m = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Local<T> local_m_param = new Local<>(new DirectRef<>(m_param, new GenericClassTag<>()), m_param.o());
        Assign<T> m_assign = new Assign<>(local_m, local_m_param, new GeneratedBlame<>(), OriGen.create());

        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(java.util.List.of(m_assign))), OriGen.create());

        // Constructor contract
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), m.o());
        FieldLocation<T> val_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(val, new GenericClassTag<>()), val.o());

        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), m.o());
        Perm<T> perm_val = new Perm<>(val_loc, new WritePerm<>(OriGen.create()), val.o());
        Eq<T> eq_m = new Eq<>(local_m, local_m_param, OriGen.create());

        ApplicableContract<T> contract = new ApplicableContract<>(new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()),
                new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perm_m, eq_m, perm_val)), OriGen.create()),
                col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(),
                OriGen.create());

        return new PVLConstructor<>(contract, params, Option.apply(body), new GeneratedBlame<>(), o);
    }

    /**
     * Creates the abstract read method for the signal class.
     *
     * @param t Signal data type
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @return Signal read method
     */
    private InstanceMethod<T> create_signal_read_method(Type<T> t, InstanceField<T> m, InstanceField<T> val) {
        Expr<T> perms = create_general_contract(m);

        // Precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());

        // Postcondition
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Deref<T> val_deref = new Deref<>(col_system.THIS, new DirectRef<>(val, new GenericClassTag<>()), new GeneratedBlame<>(), val.o());

        Ref<T, InstanceField<T>> ref_to_proc = new DirectRef<>(col_system.get_process_state(), new GenericClassTag<>());
        Ref<T, InstanceField<T>> ref_to_ev = new DirectRef<>(col_system.get_event_state(), new GenericClassTag<>());
        Deref<T> proc_deref = new Deref<>(m_deref, ref_to_proc, new GeneratedBlame<>(), col_system.get_process_state().o());
        Deref<T> ev_deref = new Deref<>(m_deref, ref_to_ev, new GeneratedBlame<>(), col_system.get_event_state().o());

        Eq<T> proc_is_old = new Eq<>(proc_deref, new Old<>(proc_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());
        Eq<T> ev_is_old = new Eq<>(ev_deref, new Old<>(ev_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());
        Eq<T> val_is_old = new Eq<>(val_deref, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Ref<T, ContractApplicable<T>> ref = new LazyRef<>(() -> col_system.get_primitive_instance_method(sc_inst, Constants.SIGNAL_READ_METHOD), Option.empty(),
                new GenericClassTag<>());
        Result<T> ret = new Result<>(ref, OriGen.create());
        Eq<T> result = new Eq<>(ret, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perms, proc_is_old, ev_is_old, val_is_old, result)),
                OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS,
                col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        return new InstanceMethod<>(t, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract, false, false,
                new GeneratedBlame<>(), OriGen.create("signal_read"));
    }

    /**
     * Creates the abstract write method for the signal class.
     *
     * @param t Signal data type
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @return Signal write method
     */
    private InstanceMethod<T> create_signal_write_method(Type<T> t, InstanceField<T> m, InstanceField<T> val) {
        Expr<T> perms = create_general_contract(m);
        Variable<T> new_val = new Variable<>(t, OriGen.create("new_val"));

        // Parameters
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(new_val)));

        // Precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());

        // Postcondition
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, new GenericClassTag<>()), new GeneratedBlame<>(), m.o());
        Deref<T> val_deref = new Deref<>(col_system.THIS, new DirectRef<>(val, new GenericClassTag<>()), new GeneratedBlame<>(), val.o());

        Ref<T, InstanceField<T>> ref_to_proc = new DirectRef<>(col_system.get_process_state(), new GenericClassTag<>());
        Ref<T, InstanceField<T>> ref_to_ev = new DirectRef<>(col_system.get_event_state(), new GenericClassTag<>());
        Deref<T> proc_deref = new Deref<>(m_deref, ref_to_proc, new GeneratedBlame<>(), col_system.get_process_state().o());
        Deref<T> ev_deref = new Deref<>(m_deref, ref_to_ev, new GeneratedBlame<>(), col_system.get_event_state().o());

        Eq<T> proc_is_old = new Eq<>(proc_deref, new Old<>(proc_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Local<T> new_val_local = new Local<>(new DirectRef<>(new_val, new GenericClassTag<>()), new_val.o());
        Eq<T> changed_val = new Eq<>(val_deref, new_val_local, OriGen.create());

        Neq<T> val_changed = new Neq<>(new_val_local, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());
        Eq<T> val_not_changed = new Eq<>(new_val_local, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        int next_event_id = col_system.get_total_nr_events() + Constants.SIGNAL_WRITE_EVENT;
        event_ids.add(next_event_id);
        SeqUpdate<T> ev_update = new SeqUpdate<>(ev_deref, new IntegerValue<>(BigInt.apply(next_event_id), OriGen.create()),
                new IntegerValue<>(BigInt.apply(-1), OriGen.create()), OriGen.create());
        Eq<T> ev_notify = new Eq<>(ev_deref, new Old<>(ev_update, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());
        Eq<T> ev_not_notify = new Eq<>(ev_deref, new Old<>(ev_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());

        Implies<T> ev_n = new Implies<>(val_changed, ev_notify, OriGen.create());
        Implies<T> ev_n_n = new Implies<>(val_not_changed, ev_not_notify, OriGen.create());

        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(java.util.List.of(perms, proc_is_old, changed_val, ev_n, ev_n_n)),
                OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS,
                col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        return new InstanceMethod<>(col_system.T_VOID, params, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract, false, false,
                new GeneratedBlame<>(), OriGen.create("signal_write"));
    }

    /**
     * Generates a general contract for all known type methods.
     *
     * @param m Main reference field
     * @return Context permission expression for all pre- and postconditions
     */
    private Expr<T> create_general_contract(InstanceField<T> m) {
        // Create references
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, new GenericClassTag<>());
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, m_ref, m.o());
        Ref<T, InstanceField<T>> self_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(), new GenericClassTag<>());
        Deref<T> self_deref = new Deref<>(m_deref, self_ref, new GeneratedBlame<>(), OriGen.create());

        // Create individual contract conditions
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Neq<T> m_not_null = new Neq<>(m_deref, col_system.NULL, OriGen.create());
        Held<T> held_m = new Held<>(m_deref, OriGen.create());
        Ref<T, InstancePredicate<T>> scheduler_perms_ref = new LazyRef<>(col_system::get_scheduler_perms, Option.empty(), new GenericClassTag<>());
        InstancePredicateApply<T> scheduler_perms = new InstancePredicateApply<>(m_deref, scheduler_perms_ref, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());
        InstancePredicateApply<T> channel_perms = new InstancePredicateApply<>(m_deref, new DirectRef<>(col_system.get_prim_channel_inv(sc_inst), new GenericClassTag<>()),
                col_system.NO_EXPRS, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> this_is_self = new Eq<>(self_deref, col_system.THIS, OriGen.create());

        // Connect the individual conditions with stars and return
        return col_system.fold_star(java.util.List.of(perm_m, m_not_null, held_m, scheduler_perms, channel_perms, this_is_self));
    }
}

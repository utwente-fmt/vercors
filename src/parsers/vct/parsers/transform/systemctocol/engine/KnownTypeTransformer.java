package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.SCClass;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import scala.Option;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.math.BigInt;
import scala.reflect.ClassTag$;
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
     * Constructor.
     *
     * @param sc_inst SystemC primitive channel instance to be transformed
     * @param col_system COL system context
     */
    public KnownTypeTransformer(SCKnownType sc_inst, COLSystem<T> col_system) {
        this.sc_inst = sc_inst;
        this.col_system = col_system;
    }

    /**
     * Generates a COL class based on the triggering Known Type instance.
     */
    public void transform() {
        // Create name for the channel class
        SCClass sc_class = sc_inst.getSCClass();
        String name = generate_class_name();

        // Transform the primitive channel
        Class<T> cls = switch (sc_class.getName()) {
            case Constants.CLASS_FIFO_INT -> transform_fifo(OriGen.create(name), col_system.T_INT);
            case Constants.CLASS_FIFO_BOOL -> transform_fifo(OriGen.create(name), col_system.T_BOOL);
            case Constants.CLASS_SIGNAL_INT -> transform_signal(OriGen.create(name), col_system.T_INT);
            case Constants.CLASS_SIGNAL_BOOL -> transform_signal(OriGen.create(name), col_system.T_BOOL);
            default -> throw new UnsupportedException("The known type " + sc_class.getName() + " is not supported.");
        };

        // Add channel class to COL system
        col_system.add_global_declaration(cls);

        // Add channel field to COL system
        Ref<T, Class<T>> ref_to_cls = new DirectRef<>(cls, ClassTag$.MODULE$.apply(Class.class));
        col_system.add_primitive_channel(sc_inst, new InstanceField<>(new TClass<>(ref_to_cls, OriGen.create()), col_system.NO_FLAGS,
                OriGen.create(name.toLowerCase())));
    }

    /**
     * Generates an appropriate name for the COL class, with capital starting letter and prefixing if necessary.
     *
     * @return A unique name for the class
     */
    private String generate_class_name() {
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
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), ClassTag$.MODULE$.apply(Class.class));
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, OriGen.create("m"));
        InstanceField<T> buf = new InstanceField<>(new TSeq<>(t, OriGen.create()), col_system.NO_FLAGS, OriGen.create("buffer"));
        InstanceField<T> nr_read = new InstanceField<>(col_system.T_INT, col_system.NO_FLAGS, OriGen.create("num_read"));
        InstanceField<T> written = new InstanceField<>(new TSeq<>(t, OriGen.create()), col_system.NO_FLAGS, OriGen.create("written"));
        col_system.add_primitive_instance_field(sc_inst, Constants.FIFO_BUFFER, buf);
        col_system.add_primitive_instance_field(sc_inst, Constants.FIFO_NUM_READ, nr_read);
        col_system.add_primitive_instance_field(sc_inst, Constants.FIFO_WRITTEN, written);

        // Permission invariant for the Main class
        InstancePredicate<T> inv = create_fifo_permission_invariant(m, buf, nr_read, written);
        col_system.add_prim_channel_inv(sc_inst, inv);

        // Class methods
        PVLConstructor<T> constructor = create_fifo_constructor(o, t, m, buf, nr_read, written);
        InstanceMethod<T> fifo_read = create_fifo_read_method(t, m, buf, nr_read, written);
        InstanceMethod<T> fifo_write = create_fifo_write_method(t, m, buf, nr_read, written);
        InstanceMethod<T> fifo_update = create_fifo_update_method(m, buf, nr_read, written);

        // Register methods in COL system
        col_system.add_primitive_instance_method(sc_inst, Constants.FIFO_READ_METHOD, fifo_read);
        col_system.add_primitive_instance_method(sc_inst, Constants.FIFO_WRITE_METHOD, fifo_write);
        col_system.add_primitive_instance_method(sc_inst, Constants.PRIMITIVE_UPDATE_METHOD_INDEX, fifo_update);

        // Create the class
        java.util.List<ClassDeclaration<T>> declarations = java.util.List.of(m, buf, nr_read, written, constructor, fifo_read, fifo_write, fifo_update);
        return new Class<>(List.from(CollectionConverters.asScala(declarations)), col_system.NO_CLS_REFS, col_system.TRUE, o);
    }

    /**
     * Creates the permission invariant for this FIFO queue, for use in the Main class.
     *
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @param nr_read Class attribute referencing the number of read elements
     * @param written Class attribute referencing the temporary buffer of items written before the delta cycle update
     * @return A predicate containing permissions and constraints for the FIFO queue
     */
    private InstancePredicate<T> create_fifo_permission_invariant(InstanceField<T> m, InstanceField<T> buf, InstanceField<T> nr_read,
                                                                  InstanceField<T> written) {
        // Create references to FIFO object
        Ref<T, InstanceField<T>> fifo_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(),
                ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> fifo_deref = new Deref<>(col_system.THIS, fifo_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> fifo_loc = new FieldLocation<>(col_system.THIS, fifo_ref, OriGen.create());

        // Create references to m attribute
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(fifo_deref, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(fifo_deref, m_ref, m.o());

        // Create references to buffer attribute
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> buf_deref = new Deref<>(fifo_deref, buf_ref, new GeneratedBlame<>(), buf.o());
        FieldLocation<T> buf_loc = new FieldLocation<>(fifo_deref, buf_ref, buf.o());
        Size<T> buf_size = new Size<>(buf_deref, OriGen.create());

        // Create references to num_read attribute
        Ref<T, InstanceField<T>> read_ref = new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_deref = new Deref<>(fifo_deref, read_ref, new GeneratedBlame<>(), nr_read.o());
        FieldLocation<T> read_loc = new FieldLocation<>(fifo_deref, read_ref, nr_read.o());

        // Create references to written attribute
        Ref<T, InstanceField<T>> written_ref = new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> written_deref = new Deref<>(fifo_deref, written_ref, new GeneratedBlame<>(), written.o());
        FieldLocation<T> written_loc = new FieldLocation<>(fifo_deref, written_ref, written.o());
        Size<T> written_size = new Size<>(written_deref, OriGen.create());

        // Create references to size parameter
        Ref<T, InstanceField<T>> fifo_size_ref = new DirectRef<>(col_system.get_fifo_size_parameter(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> fifo_size_deref = new Deref<>(col_system.THIS, fifo_size_ref, new GeneratedBlame<>(), OriGen.create());

        // Create permissions for invariant
        Perm<T> perm_fifo = new Perm<>(fifo_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), m.o());
        Perm<T> perm_buf = new Perm<>(buf_loc, new WritePerm<>(OriGen.create()), buf.o());
        Perm<T> perm_read = new Perm<>(read_loc, new WritePerm<>(OriGen.create()), nr_read.o());
        Perm<T> perm_written = new Perm<>(written_loc, new WritePerm<>(OriGen.create()), written.o());

        // Create functional properties for invariant
        Neq<T> fifo_not_null = new Neq<>(fifo_deref, col_system.NULL, OriGen.create());
        Eq<T> m_is_this = new Eq<>(m_deref, col_system.THIS, OriGen.create());
        LessEq<T> read_n_neg = new LessEq<>(col_system.ZERO, read_deref, OriGen.create());
        LessEq<T> read_in_bound = new LessEq<>(read_deref, buf_size, OriGen.create());
        Plus<T> total_size = new Plus<>(buf_size, written_size, OriGen.create());
        LessEq<T> buf_in_bound = new LessEq<>(total_size, fifo_size_deref, OriGen.create());

        // Put it all together and return
        java.util.List<Expr<T>> comps = java.util.List.of(perm_fifo, fifo_not_null, perm_m, m_is_this, perm_buf, perm_read,
                read_n_neg, read_in_bound, perm_written, buf_in_bound);
        return new InstancePredicate<>(col_system.NO_VARS, Option.apply(col_system.fold_star(comps)), false, true,
                OriGen.create(generate_class_name().toLowerCase() + "_permission_invariant"));
    }

    /**
     * Generates the FIFO class's constructor.
     *
     * @param o Class origin
     * @param t Buffer data type
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @param nr_read Class attribute referencing the number of read elements
     * @param written Class attribute referencing the temporary buffer of items written before the delta cycle update
     * @return FIFO constructor
     */
    private PVLConstructor<T> create_fifo_constructor(Origin o, Type<T> t, InstanceField<T> m, InstanceField<T> buf,
                                                      InstanceField<T> nr_read, InstanceField<T> written) {
        // Constructor parameter
        Variable<T> m_param = new Variable<>(m.t(), OriGen.create("m_param"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(m_param)));

        // Constructor body
        Deref<T> local_m = new Deref<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)),
                new GeneratedBlame<>(), m.o());
        Local<T> local_m_param = new Local<>(new DirectRef<>(m_param, ClassTag$.MODULE$.apply(Variable.class)), m_param.o());
        Assign<T> m_assign = new Assign<>(local_m, local_m_param, new GeneratedBlame<>(), OriGen.create());

        Deref<T> local_buf = new Deref<>(col_system.THIS, new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class)),
                new GeneratedBlame<>(), buf.o());
        LiteralSeq<T> empty_seq = new LiteralSeq<>(t, col_system.NO_EXPRS, OriGen.create());
        Assign<T> buf_assign = new Assign<>(local_buf, empty_seq, new GeneratedBlame<>(), OriGen.create());

        Deref<T> local_read = new Deref<>(col_system.THIS, new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class)),
                new GeneratedBlame<>(), nr_read.o());
        Assign<T> read_assign = new Assign<>(local_read, col_system.ZERO, new GeneratedBlame<>(), OriGen.create());

        Deref<T> local_written = new Deref<>(col_system.THIS, new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class)),
                new GeneratedBlame<>(), written.o());
        Assign<T> written_assign = new Assign<>(local_written, empty_seq, new GeneratedBlame<>(), OriGen.create());

        java.util.List<Statement<T>> assignments = java.util.List.of(m_assign, buf_assign, read_assign, written_assign);
        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(assignments)), OriGen.create());

        // Constructor contract
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), m.o());
        FieldLocation<T> buf_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class)), buf.o());
        FieldLocation<T> read_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class)), nr_read.o());
        FieldLocation<T> written_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class)), written.o());

        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_buf = new Perm<>(buf_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_read = new Perm<>(read_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_written = new Perm<>(written_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> eq_m = new Eq<>(local_m, local_m_param, OriGen.create());
        Eq<T> eq_buf = new Eq<>(local_buf, empty_seq, OriGen.create());
        Eq<T> eq_read = new Eq<>(local_read, col_system.ZERO, OriGen.create());
        Eq<T> eq_written = new Eq<>(local_written, empty_seq, OriGen.create());

        java.util.List<Expr<T>> conds = java.util.List.of(perm_m, eq_m, perm_buf, eq_buf, perm_read, eq_read, perm_written, eq_written);
        ApplicableContract<T> contract = new ApplicableContract<>(new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()),
                new UnitAccountedPredicate<>(col_system.fold_star(conds), OriGen.create()), col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        return new PVLConstructor<>(contract, params, Option.apply(body), new GeneratedBlame<>(), o);
    }

    /**
     * Creates the abstract read method for the FIFO queue.
     *
     * @param t Buffer data type
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @param nr_read Class attribute referencing the number of read elements
     * @param written Class attribute referencing the temporary buffer of items written before the delta cycle update
     * @return FIFO read method
     */
    private InstanceMethod<T> create_fifo_read_method(Type<T> t, InstanceField<T> m, InstanceField<T> buf, InstanceField<T> nr_read,
                                                      InstanceField<T> written) {
        Expr<T> perms = create_general_contract(m, false);

        // Get field references
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), m.o());
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> buf_deref = new Deref<>(col_system.THIS, buf_ref, new GeneratedBlame<>(), buf.o());
        Size<T> buf_size = new Size<>(buf_deref, OriGen.create());
        Ref<T, InstanceField<T>> read_ref = new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_deref = new Deref<>(col_system.THIS, read_ref, new GeneratedBlame<>(), nr_read.o());
        Ref<T, InstanceField<T>> written_ref = new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> written_deref = new Deref<>(col_system.THIS, written_ref, new GeneratedBlame<>(), written.o());

        // Get scheduling variable references
        InstanceField<T> update = col_system.get_primitive_channel_update(col_system.get_primitive_channel_id(sc_inst));
        Ref<T, InstanceField<T>> update_ref = new DirectRef<>(update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> update_deref = new Deref<>(m_deref, update_ref, new GeneratedBlame<>(), OriGen.create());

        // Create precondition
        Less<T> not_all_read = new Less<>(read_deref, buf_size, OriGen.create());
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(new Star<>(perms, not_all_read, OriGen.create()), OriGen.create());

        // Collect conditions
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged variables
        conditions.add(new Eq<>(written_deref, new Old<>(written_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        conditions.add(new Eq<>(buf_deref, new Old<>(buf_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Return value
        Ref<T, ContractApplicable<T>> ref = new LazyRef<>(() -> col_system.get_primitive_instance_method(sc_inst, Constants.FIFO_READ_METHOD),
                Option.empty(), ClassTag$.MODULE$.apply(ContractApplicable.class));
        Result<T> ret = new Result<>(ref, OriGen.create());
        SeqSubscript<T> access = new SeqSubscript<>(buf_deref, read_deref, new GeneratedBlame<>(), OriGen.create());
        conditions.add(new Eq<>(ret, new Old<>(access, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Update to num_read
        Old<T> old_read = new Old<>(read_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Plus<T> incr_old = new Plus<>(old_read, col_system.ONE, OriGen.create());
        conditions.add(new Eq<>(read_deref, incr_old, OriGen.create()));

        // Update to primitive_channel_update
        conditions.add(new Eq<>(update_deref, col_system.TRUE, OriGen.create()));
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            if (!field.equals(update)) {
                conditions.add(unchanged_field(m_deref, field));
            }
        }

        // Create postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

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
     * @param nr_read Class attribute referencing the number of read elements
     * @param written Class attribute referencing the temporary buffer of items written before the delta cycle update
     * @return FIFO write method
     */
    private InstanceMethod<T> create_fifo_write_method(Type<T> t, InstanceField<T> m, InstanceField<T> buf, InstanceField<T> nr_read,
                                                       InstanceField<T> written) {
        Expr<T> perms = create_general_contract(m, false);

        // Parameters
        Variable<T> new_val = new Variable<>(t, OriGen.create("new_val"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(new_val)));

        // Get field references
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), m.o());
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> buf_deref = new Deref<>(col_system.THIS, buf_ref, new GeneratedBlame<>(), buf.o());
        Size<T> buf_size = new Size<>(buf_deref, OriGen.create());
        Ref<T, InstanceField<T>> read_ref = new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_deref = new Deref<>(col_system.THIS, read_ref, new GeneratedBlame<>(), nr_read.o());
        Ref<T, InstanceField<T>> written_ref = new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> written_deref = new Deref<>(col_system.THIS, written_ref, new GeneratedBlame<>(), written.o());
        Size<T> written_size = new Size<>(written_deref, OriGen.create());

        // Get scheduling variable references
        InstanceField<T> update = col_system.get_primitive_channel_update(col_system.get_primitive_channel_id(sc_inst));
        Ref<T, InstanceField<T>> update_ref = new DirectRef<>(update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> update_deref = new Deref<>(m_deref, update_ref, new GeneratedBlame<>(), OriGen.create());

        // Get parameter references
        Ref<T, InstanceField<T>> fifo_size_ref = new DirectRef<>(col_system.get_fifo_size_parameter(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> fifo_size_deref = new Deref<>(m_deref, fifo_size_ref, new GeneratedBlame<>(), OriGen.create());

        // Create precondition
        Plus<T> total_size = new Plus<>(buf_size, written_size, OriGen.create());
        Less<T> within_bound = new Less<>(total_size, fifo_size_deref, OriGen.create());
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(new Star<>(perms, within_bound, OriGen.create()), OriGen.create());

        // Collect conditions
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged variables
        conditions.add(new Eq<>(read_deref, new Old<>(read_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        conditions.add(new Eq<>(buf_deref, new Old<>(buf_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Update to written
        Ref<T, Variable<T>> ref_to_new_val = new DirectRef<>(new_val, ClassTag$.MODULE$.apply(Variable.class));
        Seq<Expr<T>> literal_vals = List.from(CollectionConverters.asScala(java.util.List.of(new Local<>(ref_to_new_val, new_val.o()))));
        LiteralSeq<T> new_vals = new LiteralSeq<>(t, literal_vals, OriGen.create());
        Concat<T> concat = new Concat<>(written_deref, new_vals, OriGen.create());
        conditions.add(new Eq<>(written_deref, new Old<>(concat, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Update to primitive_channel_update
        conditions.add(new Eq<>(update_deref, col_system.TRUE, OriGen.create()));
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            if (!field.equals(update)) {
                conditions.add(unchanged_field(m_deref, field));
            }
        }

        // Create postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS, col_system.NO_VARS,
                col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new InstanceMethod<>(col_system.T_VOID, params, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract, false, false,
                new GeneratedBlame<>(), OriGen.create("fifo_write"));
    }

    /**
     * Creates the primitive channel update method for the FIFO queue.
     *
     * @param m Class attribute referencing the Main object
     * @param buf Class attribute referencing the buffer
     * @param nr_read Class attribute referencing the number of read elements
     * @param written Class attribute referencing the temporary buffer of items written before the delta cycle update
     * @return FIFO update method
     */
    private InstanceMethod<T> create_fifo_update_method(InstanceField<T> m, InstanceField<T> buf, InstanceField<T> nr_read, InstanceField<T> written) {
        Expr<T> perms = create_general_contract(m, true);

        // Get field references
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), m.o());
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(buf, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> buf_deref = new Deref<>(col_system.THIS, buf_ref, new GeneratedBlame<>(), buf.o());
        Size<T> buf_size = new Size<>(buf_deref, OriGen.create());
        Ref<T, InstanceField<T>> read_ref = new DirectRef<>(nr_read, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_deref = new Deref<>(col_system.THIS, read_ref, new GeneratedBlame<>(), nr_read.o());
        Old<T> old_read = new Old<>(read_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> written_ref = new DirectRef<>(written, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> written_deref = new Deref<>(col_system.THIS, written_ref, new GeneratedBlame<>(), written.o());
        Size<T> written_size = new Size<>(written_deref, OriGen.create());
        Old<T> old_wr_size = new Old<>(written_size, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        // Create precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged variables
        for (InstanceField<T> field : col_system.get_all_process_states()) {
            conditions.add(unchanged_field(m_deref, field));
        }
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            conditions.add(unchanged_field(m_deref, field));
        }

        // Access update sequence
        InstanceField<T> this_update = col_system.get_primitive_channel_update(col_system.get_primitive_channel_id(sc_inst));
        Ref<T, InstanceField<T>> this_update_ref = new DirectRef<>(this_update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> u_i = new Deref<>(m_deref, this_update_ref, new GeneratedBlame<>(), OriGen.create());
        Not<T> n_u_i = new Not<>(u_i, OriGen.create());

        // CASE 1: Everything is unchanged
        java.util.List<Expr<T>> case_1_effects = new java.util.ArrayList<>();
        case_1_effects.add(new Eq<>(read_deref, new Old<>(read_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        case_1_effects.add(new Eq<>(written_deref, new Old<>(written_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        case_1_effects.add(new Eq<>(buf_deref, new Old<>(buf_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        for (InstanceField<T> field : col_system.get_all_event_states()) {
            case_1_effects.add(unchanged_field(m_deref, field));
        }
        conditions.add(new Implies<>(n_u_i, col_system.fold_and(case_1_effects), OriGen.create()));

        // Preparations for CASE 2
        // Read event
        InstanceField<T> read_event_state = col_system.get_event_state(col_system.get_channel_events(sc_inst).get(Constants.FIFO_READ_EVENT));
        Ref<T, InstanceField<T>> read_event_state_ref = new DirectRef<>(read_event_state, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_event_state_deref = new Deref<>(m_deref, read_event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Old<T> old_read_event_state = new Old<>(read_event_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Greater<T> has_been_read = new Greater<>(old_read, col_system.ZERO, OriGen.create());
        Select<T> new_read_status = new Select<>(has_been_read, col_system.MINUS_ONE, old_read_event_state, OriGen.create());
        // Write event
        InstanceField<T> write_event_state = col_system.get_event_state(col_system.get_channel_events(sc_inst).get(Constants.FIFO_WRITE_EVENT));
        Ref<T, InstanceField<T>> write_event_state_ref = new DirectRef<>(write_event_state, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> write_event_state_deref = new Deref<>(m_deref, write_event_state_ref, new GeneratedBlame<>(), OriGen.create());
        Old<T> old_write_event_state = new Old<>(write_event_state_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Greater<T> has_been_written = new Greater<>(old_wr_size, col_system.ZERO, OriGen.create());
        Select<T> new_write_status = new Select<>(has_been_written, col_system.MINUS_ONE, old_write_event_state, OriGen.create());
        // Buffer
        Slice<T> buf_slice = new Slice<>(buf_deref, read_deref, buf_size, OriGen.create());
        Concat<T> buf_written = new Concat<>(buf_slice, written_deref, OriGen.create());

        // CASE 2: Update is executed
        java.util.List<Expr<T>> case_2_effects = new java.util.ArrayList<>();
        case_2_effects.add(new Eq<>(read_deref, col_system.ZERO, OriGen.create()));
        case_2_effects.add(new Eq<>(written_size, col_system.ZERO, OriGen.create()));
        case_2_effects.add(new Eq<>(buf_deref, new Old<>(buf_written, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        for (InstanceField<T> field : col_system.get_all_event_states()) {
            if (!field.equals(read_event_state) && !field.equals(write_event_state)) {
                case_2_effects.add(unchanged_field(m_deref, field));
            }
        }
        case_2_effects.add(new Eq<>(read_event_state_deref, new_read_status, OriGen.create()));
        case_2_effects.add(new Eq<>(write_event_state_deref, new_write_status, OriGen.create()));
        conditions.add(new Implies<>(u_i, col_system.fold_and(case_2_effects), OriGen.create()));

        // Create postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

        // Finish the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new InstanceMethod<>(col_system.T_VOID, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(),
                contract, false, false, new GeneratedBlame<>(), OriGen.create("fifo_update"));
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
        Ref<T, Class<T>> main_cls_ref = new LazyRef<>(col_system::get_main, Option.empty(), ClassTag$.MODULE$.apply(Class.class));
        InstanceField<T> m = new InstanceField<>(new TClass<>(main_cls_ref, OriGen.create()), col_system.NO_FLAGS, OriGen.create("m"));
        InstanceField<T> val = new InstanceField<>(t, col_system.NO_FLAGS, OriGen.create("val"));
        InstanceField<T> _val = new InstanceField<>(t, col_system.NO_FLAGS, OriGen.create("_val"));

        // Permission invariant for the Main class
        InstancePredicate<T> inv = create_signal_permission_invariant(m, val, _val);
        col_system.add_prim_channel_inv(sc_inst, inv);

        // Class methods
        PVLConstructor<T> constructor = create_signal_constructor(o, m, val, _val);
        InstanceMethod<T> signal_read = create_signal_read_method(t, m, val, _val);
        InstanceMethod<T> signal_write = create_signal_write_method(t, m, val, _val);
        InstanceMethod<T> signal_update = create_signal_update_method(m, val, _val);

        // Register methods in COL system
        col_system.add_primitive_instance_method(sc_inst, Constants.SIGNAL_READ_METHOD, signal_read);
        col_system.add_primitive_instance_method(sc_inst, Constants.SIGNAL_WRITE_METHOD, signal_write);
        col_system.add_primitive_instance_method(sc_inst, Constants.PRIMITIVE_UPDATE_METHOD_INDEX, signal_update);

        // Create the class
        java.util.List<ClassDeclaration<T>> class_content = java.util.List.of(m, val, _val, constructor, signal_read, signal_write, signal_update);
        return new Class<>(List.from(CollectionConverters.asScala(class_content)), col_system.NO_CLS_REFS, col_system.TRUE, o);
    }

    /**
     * Creates the permission invariant for this sc_signal, for use in the Main class.
     *
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @param _val Class attribute for data that was changed but not yet updated to the main data field
     * @return A predicate containing permissions and constraints for the signal instance
     */
    private InstancePredicate<T> create_signal_permission_invariant(InstanceField<T> m, InstanceField<T> val, InstanceField<T> _val) {
        // Create references to signal
        Ref<T, InstanceField<T>> signal_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(),
                ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> signal_deref = new Deref<>(col_system.THIS, signal_ref, new GeneratedBlame<>(), OriGen.create());
        FieldLocation<T> signal_loc = new FieldLocation<>(col_system.THIS, signal_ref, OriGen.create());

        // Create references to m
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(signal_deref, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(signal_deref, m_ref, m.o());

        // Create references to val and val_
        Ref<T, InstanceField<T>> val_ref = new DirectRef<>(val, ClassTag$.MODULE$.apply(InstanceField.class));
        FieldLocation<T> val_loc = new FieldLocation<>(signal_deref, val_ref, OriGen.create());
        Ref<T, InstanceField<T>> _val_ref = new DirectRef<>(_val, ClassTag$.MODULE$.apply(InstanceField.class));
        FieldLocation<T> _val_loc = new FieldLocation<>(signal_deref, _val_ref, OriGen.create());

        // Create permissions for invariant
        Perm<T> perm_signal = new Perm<>(signal_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_val = new Perm<>(val_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm__val = new Perm<>(_val_loc, new WritePerm<>(OriGen.create()), OriGen.create());

        // Create functional properties for invariant
        Neq<T> signal_not_null = new Neq<>(signal_deref, col_system.NULL, OriGen.create());
        Eq<T> m_is_this = new Eq<>(m_deref, col_system.THIS, OriGen.create());

        // Put it all together and return
        return new InstancePredicate<>(col_system.NO_VARS,
                Option.apply(col_system.fold_star(java.util.List.of(perm_signal, signal_not_null, perm_m, m_is_this, perm_val, perm__val))),
                false, true, OriGen.create(generate_class_name().toLowerCase() + "_permission_invariant"));
    }

    /**
     * Generates the signal class's constructor.
     *
     * @param o Class origin
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored value
     * @param _val Class attribute for data that was changed but not yet updated to the main data field
     * @return Signal class constructor
     */
    private PVLConstructor<T> create_signal_constructor(Origin o, InstanceField<T> m, InstanceField<T> val, InstanceField<T> _val) {
        // Constructor parameter
        Variable<T> m_param = new Variable<>(m.t(), OriGen.create("m_param"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(m_param)));

        // Constructor body
        Deref<T> local_m = new Deref<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), m.o());
        Local<T> local_m_param = new Local<>(new DirectRef<>(m_param, ClassTag$.MODULE$.apply(Variable.class)), m_param.o());
        Assign<T> m_assign = new Assign<>(local_m, local_m_param, new GeneratedBlame<>(), OriGen.create());

        Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(java.util.List.of(m_assign))), OriGen.create());

        // Constructor contract
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), m.o());
        FieldLocation<T> val_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(val, ClassTag$.MODULE$.apply(InstanceField.class)), val.o());
        FieldLocation<T> _val_loc = new FieldLocation<>(col_system.THIS, new DirectRef<>(_val, ClassTag$.MODULE$.apply(InstanceField.class)), _val.o());

        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm_val = new Perm<>(val_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Perm<T> perm__val = new Perm<>(_val_loc, new WritePerm<>(OriGen.create()), OriGen.create());
        Eq<T> eq_m = new Eq<>(local_m, local_m_param, OriGen.create());

        java.util.List<Expr<T>> conditions = java.util.List.of(perm_m, eq_m, perm_val, perm__val);
        ApplicableContract<T> contract = new ApplicableContract<>(new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()),
                new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create()), col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new PVLConstructor<>(contract, params, Option.apply(body), new GeneratedBlame<>(), o);
    }

    /**
     * Creates the abstract read method for the signal class.
     *
     * @param t Signal data type
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @param _val Class attribute for data that was changed but not yet updated to the main data field
     * @return Signal read method
     */
    private InstanceMethod<T> create_signal_read_method(Type<T> t, InstanceField<T> m, InstanceField<T> val, InstanceField<T> _val) {
        Expr<T> perms = create_general_contract(m, false);

        // Precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());

        // Get references to relevant fields
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), m.o());
        Deref<T> val_deref = new Deref<>(col_system.THIS, new DirectRef<>(val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), val.o());
        Deref<T> _val_deref = new Deref<>(col_system.THIS, new DirectRef<>(_val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), _val.o());

        // Collect conditions
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged variables
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            conditions.add(unchanged_field(m_deref, field));
        }
        conditions.add(new Eq<>(val_deref, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));
        conditions.add(new Eq<>(_val_deref, new Old<>(_val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Method return value
        Ref<T, ContractApplicable<T>> ref = new LazyRef<>(() -> col_system.get_primitive_instance_method(sc_inst, Constants.SIGNAL_READ_METHOD),
                Option.empty(), ClassTag$.MODULE$.apply(ContractApplicable.class));
        Result<T> ret = new Result<>(ref, OriGen.create());
        conditions.add(new Eq<>(ret, new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create()));

        // Postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

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
     * @param _val Class attribute for data that was changed but not yet updated to the main data field
     * @return Signal write method
     */
    private InstanceMethod<T> create_signal_write_method(Type<T> t, InstanceField<T> m, InstanceField<T> val, InstanceField<T> _val) {
        Expr<T> perms = create_general_contract(m, false);

        // Parameters
        Variable<T> new_val = new Variable<>(t, OriGen.create("new_val"));
        List<Variable<T>> params = List.from(CollectionConverters.asScala(java.util.List.of(new_val)));

        // Precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());

        // Get references to relevant fields
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), m.o());
        Deref<T> val_deref = new Deref<>(col_system.THIS, new DirectRef<>(val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), val.o());
        Old<T> old_val = new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Deref<T> _val_deref = new Deref<>(col_system.THIS, new DirectRef<>(_val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), _val.o());
        Local<T> new_val_local = new Local<>(new DirectRef<>(new_val, ClassTag$.MODULE$.apply(Variable.class)), new_val.o());

        // Collect conditions
        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged variables
        conditions.add(new Eq<>(val_deref, old_val, OriGen.create()));

        // Changed _val
        conditions.add(new Eq<>(_val_deref, new_val_local, OriGen.create()));

        // Prepare for update
        InstanceField<T> update = col_system.get_primitive_channel_update(col_system.get_primitive_channel_id(sc_inst));
        Ref<T, InstanceField<T>> update_ref = new DirectRef<>(update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> update_deref = new Deref<>(m_deref, update_ref, new GeneratedBlame<>(), OriGen.create());
        Eq<T> updated = new Eq<>(update_deref, col_system.TRUE, OriGen.create());
        Eq<T> not_updated = new Eq<>(update_deref, new Old<>(update_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create()), OriGen.create());
        Neq<T> val_changed = new Neq<>(new_val_local, old_val, OriGen.create());
        Eq<T> val_not_changed = new Eq<>(new_val_local, old_val, OriGen.create());

        // primitive_channel_update is set if the stored value changed
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            if (!field.equals(update)) {
                conditions.add(unchanged_field(m_deref, field));
            }
        }
        conditions.add(new Implies<>(val_changed, updated, OriGen.create()));
        conditions.add(new Implies<>(val_not_changed, not_updated, OriGen.create()));

        // Postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

        // Finishing the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new InstanceMethod<>(col_system.T_VOID, params, col_system.NO_VARS, col_system.NO_VARS, Option.empty(), contract,
                false, false, new GeneratedBlame<>(), OriGen.create("signal_write"));
    }

    /**
     * Creates the update method for the signal class.
     *
     * @param m Class attribute referencing the Main object
     * @param val Class attribute referencing the stored data
     * @param _val Class attribute for data that was changed but not yet updated to the main data field
     * @return Signal update method
     */
    private InstanceMethod<T> create_signal_update_method(InstanceField<T> m, InstanceField<T> val, InstanceField<T> _val) {
        Expr<T> perms = create_general_contract(m, true);

        // Get field references
        Deref<T> m_deref = new Deref<>(col_system.THIS, new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), m.o());
        Deref<T> val_deref = new Deref<>(col_system.THIS, new DirectRef<>(val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), val.o());
        Old<T> old_val = new Old<>(val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        Deref<T> _val_deref = new Deref<>(col_system.THIS, new DirectRef<>(_val, ClassTag$.MODULE$.apply(InstanceField.class)), new GeneratedBlame<>(), _val.o());
        Old<T> old__val = new Old<>(_val_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());

        // Precondition
        AccountedPredicate<T> precondition = new UnitAccountedPredicate<>(perms, OriGen.create());

        java.util.List<Expr<T>> conditions = new java.util.ArrayList<>();
        conditions.add(perms);

        // Unchanged fields
        for (InstanceField<T> field : col_system.get_all_process_states()) {
            conditions.add(unchanged_field(m_deref, field));
        }
        for (InstanceField<T> field : col_system.get_all_primitive_channel_updates()) {
            conditions.add(unchanged_field(m_deref, field));
        }
        conditions.add(new Eq<>(_val_deref, old__val, OriGen.create()));

        // Access update field
        InstanceField<T> this_update = col_system.get_primitive_channel_update(col_system.get_primitive_channel_id(sc_inst));
        Ref<T, InstanceField<T>> this_update_ref = new DirectRef<>(this_update, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> u_i = new Deref<>(m_deref, this_update_ref, new GeneratedBlame<>(), OriGen.create());
        Not<T> n_u_i = new Not<>(u_i, OriGen.create());

        // CASE 1: Everything is unchanged
        java.util.List<Expr<T>> case_1_changes = new java.util.ArrayList<>();
        for (InstanceField<T> field : col_system.get_all_event_states()) {
            case_1_changes.add(unchanged_field(m_deref, field));
        }
        case_1_changes.add(new Eq<>(val_deref, old_val, OriGen.create()));
        conditions.add(new Implies<>(n_u_i, col_system.fold_and(case_1_changes), OriGen.create()));

        // Get write event
        InstanceField<T> write_event_state = col_system.get_event_state(col_system.get_channel_events(sc_inst).get(Constants.SIGNAL_WRITE_EVENT));
        Ref<T, InstanceField<T>> write_event_state_ref = new DirectRef<>(write_event_state, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> write_event_state_deref = new Deref<>(m_deref, write_event_state_ref, new GeneratedBlame<>(), OriGen.create());

        // CASE 2: Value changed and write event notified
        java.util.List<Expr<T>> case_2_changes = new java.util.ArrayList<>();
        for (InstanceField<T> field : col_system.get_all_event_states()) {
            if (!field.equals(write_event_state)) {
                case_2_changes.add(unchanged_field(m_deref, field));
            }
        }
        case_2_changes.add(new Eq<>(write_event_state_deref, col_system.MINUS_ONE, OriGen.create()));
        case_2_changes.add(new Eq<>(val_deref, old__val, OriGen.create()));
        conditions.add(new Implies<>(u_i, col_system.fold_and(case_2_changes), OriGen.create()));

        // Postcondition
        AccountedPredicate<T> postcondition = new UnitAccountedPredicate<>(col_system.fold_star(conditions), OriGen.create());

        // Finish the method
        ApplicableContract<T> contract = new ApplicableContract<>(precondition, postcondition, col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new InstanceMethod<>(col_system.T_VOID, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS, Option.empty(),
                contract, false, false, new GeneratedBlame<>(), OriGen.create("signal_update"));
    }

    /**
     * Generates a general contract for all known type methods.
     *
     * @param m Main reference field
     * @param include_scheduler_permissions Flag indicating whether the contract should include permissions to all
     *                                      scheduling variables or only to <code>primitive_channel_update</code>
     * @return Context permission expression for all pre- and postconditions
     */
    private Expr<T> create_general_contract(InstanceField<T> m, boolean include_scheduler_permissions) {
        // Create references
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), m.o());
        FieldLocation<T> m_loc = new FieldLocation<>(col_system.THIS, m_ref, m.o());
        Ref<T, InstanceField<T>> self_ref = new LazyRef<>(() -> col_system.get_primitive_channel(sc_inst), Option.empty(),
                ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> self_deref = new Deref<>(m_deref, self_ref, new GeneratedBlame<>(), OriGen.create());

        // Create individual contract conditions
        Perm<T> perm_m = new Perm<>(m_loc, new ReadPerm<>(OriGen.create()), OriGen.create());
        Neq<T> m_not_null = new Neq<>(m_deref, col_system.NULL, OriGen.create());
        Held<T> held_m = new Held<>(m_deref, OriGen.create());
        Eq<T> this_is_self = new Eq<>(self_deref, col_system.THIS, OriGen.create());

        // Get permission predicates
        Ref<T, InstancePredicate<T>> perm_inv;
        if (include_scheduler_permissions) {
            perm_inv = new LazyRef<>(col_system::get_scheduler_perms, Option.empty(), ClassTag$.MODULE$.apply(InstancePredicate.class));
        }
        else {
            perm_inv = new LazyRef<>(col_system::get_update_perms, Option.empty(), ClassTag$.MODULE$.apply(InstancePredicate.class));
        }
        Ref<T, InstancePredicate<T>> param_inv = new LazyRef<>(col_system::get_parameter_perms, Option.empty(),
                ClassTag$.MODULE$.apply(InstancePredicate.class));

        // Apply predicates
        InstancePredicateApply<T> scheduler_perms = new InstancePredicateApply<>(m_deref, perm_inv, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());
        InstancePredicateApply<T> parameter_perms = new InstancePredicateApply<>(m_deref, param_inv, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());
        Ref<T, InstancePredicate<T>> channel_inv = new DirectRef<>(col_system.get_prim_channel_inv(sc_inst), ClassTag$.MODULE$.apply(InstancePredicate.class));
        InstancePredicateApply<T> channel_perms = new InstancePredicateApply<>(m_deref, channel_inv, col_system.NO_EXPRS,
                new WritePerm<>(OriGen.create()), OriGen.create());

        // Connect the individual conditions with stars and return
        return col_system.fold_star(java.util.List.of(perm_m, m_not_null, held_m, scheduler_perms, parameter_perms, channel_perms, this_is_self));
    }

    /**
     * Helper method that returns the equation to indicate that a field is unchanged by a method, i.e. is equal to its
     * \old value.
     *
     * @param m_deref Expression that references the main instance
     * @param field Field that should be unchanged
     * @return Equation of the form <code>m.field == \old(m.field)</code>
     */
    private Eq<T> unchanged_field(Deref<T> m_deref, InstanceField<T> field) {
        Ref<T, InstanceField<T>> f_ref = new DirectRef<>(field, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> f_deref = new Deref<>(m_deref, f_ref, new GeneratedBlame<>(), OriGen.create());
        Old<T> f_old = new Old<>(f_deref, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        return new Eq<>(f_deref, f_old, OriGen.create());
    }
}

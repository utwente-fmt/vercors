package vct.parsers.transform.systemctocol.engine;

import de.tub.pes.syscir.sc_model.*;
import de.tub.pes.syscir.sc_model.expressions.*;
import de.tub.pes.syscir.sc_model.variables.SCClassInstance;
import de.tub.pes.syscir.sc_model.variables.SCEvent;
import de.tub.pes.syscir.sc_model.variables.SCKnownType;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;
import scala.math.BigInt;
import scala.reflect.ClassTag$;
import vct.col.ast.*;
import vct.col.ref.DirectRef;
import vct.col.ref.LazyRef;
import vct.col.ref.Ref;
import vct.parsers.transform.systemctocol.exceptions.ExpressionParseException;
import vct.parsers.transform.systemctocol.exceptions.IllegalOperationException;
import vct.parsers.transform.systemctocol.exceptions.SystemCFormatException;
import vct.parsers.transform.systemctocol.exceptions.UnsupportedException;
import vct.parsers.transform.systemctocol.colmodel.COLClass;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.colmodel.ProcessClass;
import vct.parsers.transform.systemctocol.util.Constants;
import vct.parsers.transform.systemctocol.util.GeneratedBlame;
import vct.parsers.transform.systemctocol.util.OriGen;
import vct.parsers.transform.systemctocol.util.Timing;

/*
This class is responsible for translating SystemC intermediate representation expressions to either COL statements, if
the expression in question encodes a program statement, or to COL expressions if the expression encodes a variable or a
value. However, not all expressions are supported. Some are at odds with our encoding, some encode behavior that we
cannot support (i.e. dynamic process creation), some are irrelevant for our encoding and some are handled manually
elsewhere and do not need to be explicitly translated. Likewise, some expressions can be translated multiple ways. A
function call, for instance, could be an expression in the program or a statement on its own.

The table below shows all SysCIR expressions except the abstract Expression, LoopExpression and MarkerExpression and our
support for translating them to statements and expressions. v indicates full support, (v) means that the expression is
supported but is translated to null, and an empty space means that the expression will throw an exception. Indented
expressions are those with no support at all.

        SYSCIR EXPRESSION           STATEMENT   EXPRESSION

    AccessExpression                    v           v
    ArrayAccessExpression                           v
       ArrayInitializerExpression
    AssertionExpression                 v
    BinaryExpression                    v           v
    BracketExpression                               v
    BreakExpression                     v
       CaseExpression
    ConstantExpression                              v
    ContinueExpression                  v
    DeleteArrayExpression              (v)
    DeleteExpression                   (v)
    DoWhileLoopExpression               v
    EmptyExpression                                (v)
       EndlineExpression
    EnumElementExpression                           v
    EventNotificationExpression         v
    ExpressionBlock                     v
    ForLoopExpression                   v
    FunctionCallExpression              v           v
       GoalAnnotation
    GoToExpression                      v
    IfElseExpression                    v
       MultiSocketAccessExpression
       NameExpression
    NewArrayExpression                              v
       NewExpression
    OutputExpression                   (v)
    QuestionMarkExpression                          v
    RefDerefExpression                              v
    ReturnExpression                    v
       SCClassInstanceExpression
       SCDeltaCountExpression
    SCPortSCSocketExpression                        v
    SCStopExpression                   (v)
       SCTimeStampExpression
    SCVariableDeclarationExpression     v
    SCVariableExpression                            v
    SCVariableNonDetSet                 v
       SocketFunctionCallExpression
    SwitchExpression                    v
       TimeUnitExpression
    UnaryExpression                                 v
    WhileLoopExpression                 v
 */

/**
 * Transforms SystemC expressions to COL statements or expressions with equivalent semantics in our encoding.
 *
 * @param <T> IGNORED
 */
public class ExpressionTransformer<T> {

    /**
     * Main reference field of the containing class.
     */
    private final InstanceField<T> m;

    /**
     * COL system context.
     */
    private final COLSystem<T> col_system;

    /**
     * Class that contains the expressions to be converted.
     */
    private final COLClass col_class;

    /**
     * Process class that uses the expressions to be converted.
     */
    private final ProcessClass corr_proc;

    /**
     * A map from SystemC variables to COL local variables that have already been resolved, e.g. method parameters.
     */
    private final java.util.Map<SCVariable, Variable<T>> local_variables;

    /**
     * A list of methods that were generated in the process of transforming these expressions and should therefore also
     * be added to the containing class, e.g. helper functions for generating random values.
     */
    private final java.util.List<InstanceMethod<T>> newly_generated_methods;

    /**
     * A flag indicating whether the containing method can be pure given the translated expressions. This is always true
     * at the start and is set to false once a statement is encountered that can have a side effect.
     */
    private boolean pure;

    public ExpressionTransformer(InstanceField<T> m, COLSystem<T> col_system, COLClass col_class, ProcessClass corr_proc,
                                 java.util.Map<SCVariable, Variable<T>> local_variables) {
        this.m = m;
        this.col_system = col_system;
        this.col_class = col_class;
        this.corr_proc = corr_proc;
        this.local_variables = local_variables;
        this.newly_generated_methods = new java.util.ArrayList<>();
        this.pure = true;
    }

    // ============================================================================================================== //
    // ============================= TRANSFORM FROM SYSTEMC EXPRESSION TO COL STATEMENT ============================= //
    // ============================================================================================================== //

    /**
     * Takes a SystemC executable expression and converts it to a COL statement. If the given expression is not
     * executable, it throws an IllegalArgumentException.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @return A COL statement (possibly a block of multiple statements) encoding the semantics of the given expression
     */
    public Statement<T> create_statement(Expression expr, SCClassInstance sc_inst) {
        return create_statement(expr, sc_inst, col_system.THIS, col_system.TRUE);
    }

    /**
     * Transforms the given expression to COL, given that it is operating on the given object. This represents an
     * intermediate step in the transformation process, in which the resulting statement should be a member of the given
     * object.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> create_statement(Expression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        if (expr == null) {
            return null;
        }
        if (expr instanceof AccessExpression e) {
            return transform_access_expression_to_statement(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof AssertionExpression e) {
            return transform_assertion_expression(e, sc_inst, obj);
        }
        if (expr instanceof BinaryExpression e) {
            pure = false;
            return transform_binary_expression_to_statement(e, sc_inst, obj);
        }
        if (expr instanceof BreakExpression e) {
            return transform_break_expression(e);
        }
        if (expr instanceof ContinueExpression e) {
            return transform_continue_expression(e);
        }
        if (expr instanceof DeleteArrayExpression) {
            pure = false;
            return null;    // TODO: Do we need to handle delete expressions?
        }
        if (expr instanceof DeleteExpression) {
            pure = false;
            return null;    // TODO: Do we need to handle delete expressions?
        }
        if (expr instanceof DoWhileLoopExpression e) {
            pure = false;
            return transform_do_while_loop_expression(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof EventNotificationExpression e) {
            pure = false;
            return transform_event_notification_expression(e, sc_inst);
        }
        if (expr instanceof ExpressionBlock e) {
            return transform_expression_block(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof ForLoopExpression e) {
            pure = false;
            return transform_for_loop_expression(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof FunctionCallExpression e) {
            pure = false;
            return transform_function_call_expression_to_statement(e, sc_inst, obj);
        }
        if (expr instanceof GoToExpression e) {
            return transform_go_to_expression(e);
        }
        if (expr instanceof IfElseExpression e) {
            return transform_if_else_expression(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof OutputExpression) {
            // Ignore output
            return null;
        }
        if (expr instanceof ReturnExpression e) {
            return transform_return_expression(e, sc_inst, obj);
        }
        if (expr instanceof SCStopExpression) {
            // Ignore sc_stop, since we don't support reasoning over simulation time    TODO: Should we?
            return null;
        }
        if (expr instanceof SCVariableDeclarationExpression e) {
            // Since local variables are declared as class attributes, local variable declarations make a method impure
            pure = false;
            return transform_sc_variable_declaration_expression(e, sc_inst, obj);
        }
        if (expr instanceof SCVariableNonDetSet e) {
            pure = false;
            return transform_sc_variable_nondet_set(e, sc_inst, obj);
        }
        if (expr instanceof SwitchExpression e) {
            return transform_switch_expression(e, sc_inst, obj, path_cond);
        }
        if (expr instanceof WhileLoopExpression e) {
            pure = false;
            return transform_while_loop_expression(e, sc_inst, obj, path_cond);
        }
        // TODO: SocketFunctionCallExpression, MultiSocketAccessExpression
        throw new ExpressionParseException("The following statement is not supported:\n\n" + expr, expr);
    }

    /**
     * Creates a statement from an access expression of the form <code>object.[other refs].function();</code>. If the
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> transform_access_expression_to_statement(AccessExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        Statement<T> result;
        // FIFO calls require prior waiting, so handle them differently
        if (expr.getRight() instanceof FunctionCallExpression fun_expr && expr.getLeft() instanceof SCPortSCSocketExpression sc_port
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port.getSCPortSCSocket().getConType())) {
            result = transform_fifo_call_expression(sc_port, fun_expr, sc_inst, obj, null, "");
        }
        // Signal calls cannot be translated directly, since their methods are not direct translations of the SystemC functions
        else if (expr.getRight() instanceof FunctionCallExpression fun_expr && expr.getLeft() instanceof SCPortSCSocketExpression sc_port
                && SCPORTSCSOCKETTYPE.SC_SIGNAL_ALL.contains(sc_port.getSCPortSCSocket().getConType())) {
            result = transform_signal_call_expression_to_statement(sc_port, fun_expr, sc_inst, obj, null, "");
        }
        else {
            SCClassInstance next_inst = sc_inst;
            if (expr.getLeft() instanceof SCClassInstanceExpression cls_inst_expr) {
                next_inst = cls_inst_expr.getInstance();
            }
            if (expr.getLeft() instanceof SCPortSCSocketExpression sc_port_expr) {
                SCPort sc_port = sc_port_expr.getSCPortSCSocket();
                SCClassInstance connected_channel = col_system.get_hierarchical_port_connection(sc_inst, sc_port);
                if (connected_channel != null) {
                    next_inst = connected_channel;
                }
            }
            Expr<T> left = create_expression(expr.getLeft(), sc_inst, obj);
            result = create_statement(expr.getRight(), next_inst, left, path_cond);
        }

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms an assertion from the SystemC system to an assertion in the COL system.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return Assertion in the COL system
     */
    private Statement<T> transform_assertion_expression(AssertionExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);
        Statement<T> result = new Assert<>(cond, new GeneratedBlame<>(), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Converts an assignment in the SystemC system (encoded as a binary expression with "=" as the operator) to one in
     * the COL system.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return Corresponding assignment in the COL system
     */
    private Statement<T> transform_binary_expression_to_statement(BinaryExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Statement<T> result;

        // Create the variable expression that is on the left hand side of the assign
        Expr<T> assign_to = create_expression(expr.getLeft(), sc_inst, obj);

        // FIFO calls require prior waiting, so handle them separately (only right side is checked, since this must be an assignment)
        if (expr.getRight() instanceof AccessExpression acc_e && acc_e.getLeft() instanceof SCPortSCSocketExpression sc_port
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port.getSCPortSCSocket().getConType())
                && acc_e.getRight() instanceof FunctionCallExpression fun_expr) {
            result = transform_fifo_call_expression(sc_port, fun_expr, sc_inst, obj, assign_to, expr.getOp());
        }
        // Signal calls cannot be translated directly, since their methods are not direct translations of the SystemC functions
        else if (expr.getRight() instanceof AccessExpression acc_e && acc_e.getLeft() instanceof SCPortSCSocketExpression sc_port
                && SCPORTSCSOCKETTYPE.SC_SIGNAL_ALL.contains(sc_port.getSCPortSCSocket().getConType())
                && acc_e.getRight() instanceof FunctionCallExpression fun_expr) {
            result = transform_signal_call_expression_to_statement(sc_port, fun_expr, sc_inst, obj, assign_to, expr.getOp());
        }
        // Otherwise simply transform the assign
        else {
            Expr<T> right = create_expression(expr.getRight(), sc_inst, obj);
            result = decode_assignment(assign_to, expr.getOp(), right);
        }

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a break statement into COL.
     *
     * @param expr Expression to be converted
     * @return A COL break statement
     */
    private Statement<T> transform_break_expression(BreakExpression expr) {
        Statement<T> result = new Break<>(Option.empty(), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a continue statement into COL.
     *
     * @param expr Expression to be converted
     * @return A COL continue statement
     */
    private Statement<T> transform_continue_expression(ContinueExpression expr) {
        Statement<T> result = new Continue<>(Option.empty(), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a do-while loop in SystemC into a COL loop statement. Since there is no do-while in COL, it instead
     * unrolls the loop once.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A block containing first the body of the loop and then a translation of the rest of the loop
     */
    private Statement<T> transform_do_while_loop_expression(DoWhileLoopExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);

        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        LoopContract<T> contract = specification_transformer.create_loop_invariant(path_cond);

        Block<T> body = expression_list_to_block(expr.getLoopBody(), sc_inst, obj, new And<>(path_cond, cond, OriGen.create()));
        if (body == null) return null;
        Loop<T> loop = new Loop<>(col_system.get_empty_block(), cond, col_system.get_empty_block(), contract, body, OriGen.create());

        java.util.List<Statement<T>> stmts = java.util.List.of(body, loop);
        Statement<T> result = new Block<>(List.from(CollectionConverters.asScala(stmts)), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms an event notification expression into an update of the encoded scheduling state.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @return An update to the event state
     */
    private Statement<T> transform_event_notification_expression(EventNotificationExpression expr, SCClassInstance sc_inst) {
        if (expr.getEvent() instanceof SCVariableExpression var_expr) {
            if (var_expr.getVar() instanceof SCEvent event) {
                // Find corresponding event ID
                Expr<T> event_id = new IntegerValue<>(BigInt.apply(col_system.get_shared_event(sc_inst, event)), OriGen.create());

                // Find wait time
                Expr<T> wait_time;
                switch (expr.getParameters().size()) {
                    case 0 -> wait_time = col_system.ZERO;
                    case 1 -> wait_time = col_system.MINUS_ONE;
                    case 2 -> {
                        long total_delay = Timing.getTimeUnits(expr.getParameters().get(0).toStringNoSem(),
                                expr.getParameters().get(1).toStringNoSem());
                        if (total_delay == 0) wait_time = col_system.MINUS_ONE;
                        else wait_time = new IntegerValue<>(BigInt.apply(total_delay), OriGen.create());
                    }
                    default -> throw new SystemCFormatException("Event notification with more than 2 parameters!");
                }

                // Create reference to event sequence
                Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
                Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());
                Ref<T, InstanceField<T>> event_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
                Deref<T> events_deref = new Deref<>(m_deref, event_ref, new GeneratedBlame<>(), OriGen.create());

                // Create sequence update
                SeqUpdate<T> update_event = new SeqUpdate<>(events_deref, event_id, wait_time, OriGen.create());
                Statement<T> result = new Assign<>(events_deref, update_event, new GeneratedBlame<>(), OriGen.create());

                // Handle label
                return append_label(result, expr);
            }
            else throw new SystemCFormatException("Notified variable " + var_expr + " is not an event!");
        }
        else throw new SystemCFormatException("Notified event " + expr.getEvent().toStringNoSem() + " is not a variable!");   // TODO: Handle nested variables!
    }

    /**
     * Converts a block of expressions to an expression block in COL.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> transform_expression_block(ExpressionBlock expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        Statement<T> result = expression_list_to_block(expr.getBlock(), sc_inst, obj, path_cond);

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Converts a for loop to a COL loop.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL loop with the same semantics as the given SystemC for loop
     */
    private Statement<T> transform_for_loop_expression(ForLoopExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        Statement<T> init = create_statement(expr.getInitializer(), sc_inst, obj, path_cond);
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);
        Statement<T> update = create_statement(expr.getIterator(), sc_inst, obj, path_cond);

        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        LoopContract<T> contract = specification_transformer.create_loop_invariant(path_cond);

        Block<T> body = expression_list_to_block(expr.getLoopBody(), sc_inst, obj, new And<>(path_cond, cond, OriGen.create()));
        if (body == null) return null;
        Statement<T> result = new Loop<>(init, cond, update, contract, body, OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a function call in the SystemC system. If the function is an internal function (e.g. random) and does
     * not yet exist, creates a new function and calls it.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return A COL method invocation on the COL equivalent of the SystemC method
     */
    private Statement<T> transform_function_call_expression_to_statement(FunctionCallExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        if (expr.getFunction().getName().equals("wait")) return transform_wait_expression(expr, sc_inst, obj);

        // Find appropriate COL function (or reference thereto)
        SCFunction sc_fun = expr.getFunction();
        // If the function is a random function, generate a new one
        Ref<T, InstanceMethod<T>> col_fun = switch (sc_fun.getName()) {
            case "rand", "rand_r", "random" -> create_random_function(col_system.T_INT);
            case "srand", "sc_module" -> null;
            default -> new LazyRef<>(() -> col_system.get_instance_method(sc_fun, sc_inst, corr_proc), Option.empty(),
                    ClassTag$.MODULE$.apply(InstanceMethod.class));
        };

        // Ignore the srand function and the sc_module constructor call
        if (col_fun == null) return null;

        // Transform parameter list
        java.util.List<Expr<T>> arguments = new java.util.ArrayList<>();
        for (Expression param : expr.getParameters()) {
            Expr<T> parameter = create_expression(param, sc_inst, obj);
            if (parameter == null) throw new ExpressionParseException("Function call parameter " + param + " could not be parsed!", param);
            arguments.add(parameter);
        }

        Statement<T> result;

        // If the variable is an attribute of this class, but not of the corresponding COL class, access it through the containing instance
        COLClass containing_class = col_system.get_method_containing_class(sc_fun, sc_inst);        // Not null, since the state class is transformed first
        if (obj == col_system.THIS && !containing_class.equals(col_class)) {
            // Create m reference
            Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

            // Create reference to the instance of the class containing the variable
            Ref<T, InstanceField<T>> containing_instance = new LazyRef<>(() -> col_system.get_instance_by_class(containing_class),
                    Option.empty(), ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> containing_deref = new Deref<>(m_deref, containing_instance, new GeneratedBlame<>(), OriGen.create());

            result = new InvokeMethod<>(containing_deref, col_fun, List.from(CollectionConverters.asScala(arguments)), col_system.NO_EXPRS,
                    col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        }
        // Else invoke the method on the given object (might be this)
        else {
            result = new InvokeMethod<>(obj, col_fun, List.from(CollectionConverters.asScala(arguments)), col_system.NO_EXPRS,
                    col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        }

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a goto expression.
     *
     * @param expr Expression to be converted
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> transform_go_to_expression(GoToExpression expr) {
        String jump = expr.getJumpLabel();
        Ref<T, LabelDecl<T>> label = new LazyRef<>(() -> col_system.get_label(jump), Option.empty(), ClassTag$.MODULE$.apply(LabelDecl.class));
        Goto<T> result = new Goto<>(label, OriGen.create(jump));

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms an if-else expression into the COL branch statement. If a branch is empty (e.g. because it only
     * contains output expressions), it does not appear in the result. If both branches are empty, the result is null.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL branch statement with equivalent semantics as the
     */
    private Statement<T> transform_if_else_expression(IfElseExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        // Create if condition (and its negation for the else branch)
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);
        if (cond == null) throw new ExpressionParseException("If expression condition could not be converted!", expr);
        Expr<T> n_cond = new Not<>(cond, OriGen.create());

        // Create if and else branches
        Block<T> then_body = expression_list_to_block(expr.getThenBlock(), sc_inst, obj, new And<>(path_cond, cond, OriGen.create()));
        Block<T> else_body = expression_list_to_block(expr.getElseBlock(), sc_inst, obj, new And<>(path_cond, n_cond, OriGen.create()));

        // Create branches
        java.util.List<Tuple2<Expr<T>, Statement<T>>> branches = new java.util.ArrayList<>();
        if (then_body != null) branches.add(new Tuple2<>(cond, then_body));
        if (else_body != null) branches.add(new Tuple2<>(n_cond, else_body));

        // Assemble the branch statement
        if (branches.isEmpty()) return null;
        Statement<T> result = new Branch<>(List.from(CollectionConverters.asScala(branches)), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a return expression.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return A COL return statement encoding the semantics of the given expression
     */
    private Statement<T> transform_return_expression(ReturnExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Expr<T> return_value = create_expression(expr.getReturnStatement(), sc_inst, obj);
        if (return_value == null) return_value = col_system.VOID;
        Statement<T> result = new Return<>(return_value, OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a SystemC variable declaration. Since local variables are transformed to class attributes in the COL
     * encoding, this just initializes the variable if the declaration also initializes it. If it does not, the
     * declaration is ignored and this method returns <code>null</code>.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> transform_sc_variable_declaration_expression(SCVariableDeclarationExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Statement<T> result = null;

        if (expr.getVariable() instanceof SCVariableExpression var_expr) {
            // Only do anything if the variable is initialized; otherwise, its declaration will be as an attribute of the surrounding class
            if (var_expr.getVar().hasInitialValue()) {
                // Translate variable
                Expr<T> var = create_expression(var_expr, sc_inst, obj);

                // Translate initial value
                Expr<T> init_val;
                if (expr.getInitialValues().size() == 1) {
                    init_val = create_expression(expr.getFirstInitialValue(), sc_inst, obj);
                }
                else throw new UnsupportedException("Initialized variable " + var_expr.getVar() + " is of unsupported type.", expr);

                result = new Assign<>(var, init_val, new GeneratedBlame<>(), OriGen.create());
            }
        }
        else if (expr.getVariable() instanceof SCClassInstanceExpression) {
            throw new UnsupportedException("Local declaration of class instances is not supported!", expr);      // TODO: Support dynamic creation at least for structs?
        }

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Encodes a nondeterministic assignment to a variable. Creates a new random function and returns an assignment of
     * the function return value to the variable.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @return A COL statement encoding the semantics of the given expression
     */
    private Statement<T> transform_sc_variable_nondet_set(SCVariableNonDetSet expr, SCClassInstance sc_inst, Expr<T> obj) {
        Expr<T> var = create_expression(expr.getVar(), sc_inst, obj);

        // Create a new random method
        Ref<T, InstanceMethod<T>> randomizer = create_random_function(col_system.parse_type(expr.getVar().getVar().getType()));

        // Create method invocation of generated method and assign its value to the variable
        MethodInvocation<T> randomize = new MethodInvocation<>(col_system.THIS, randomizer, col_system.NO_EXPRS, col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        Statement<T> result = new Assign<>(var, randomize, new GeneratedBlame<>(), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a switch expression to a COL branch statement. Empty branches are ignored. If there are no viable
     * branches, this method returns <code>null</code>. The transformation ignores break expressions. Break expressions
     * are not allowed as internal statements, e.g. in if-else expressions or loops.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL branch statement encoding the semantics of the given switch expression
     */
    private Statement<T> transform_switch_expression(SwitchExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {

        // Collect components of the switch statement
        Expr<T> cond = create_expression(expr.getSwitchExpression(), sc_inst, obj);
        if (cond == null) throw new ExpressionParseException("Could not parse switch expression " + expr.getSwitchExpression(), expr);
        java.util.List<Expression> cases = expr.getCases();

        // Prepare data structures
        java.util.List<Tuple2<Expr<T>, Statement<T>>> branches = new java.util.ArrayList<>();
        java.util.Map<CaseExpression, Expr<T>> conditions = new java.util.HashMap<>();
        CaseExpression default_case = null;

        // First, create the condition of each case expression
        for (Expression e : cases) {
            CaseExpression case_expr = (CaseExpression) e;

            // The default case has the special condition that none of the other conditions are fulfilled and cannot be created yet
            if (case_expr.isDefaultCase()) {
                default_case = case_expr;
            }
            // For any other cases, create a pair of condition and body and add it to the list
            else {
                Expr<T> case_cond = create_expression(case_expr.getCondition(), sc_inst, obj);
                if (case_cond == null) throw new ExpressionParseException("Could not parse case expression " + e, e);
                Eq<T> branch_cond = new Eq<>(cond, case_cond, OriGen.create());
                conditions.put(case_expr, branch_cond);
            }
        }

        // The default case is the negation of all other cases
        if (default_case != null) {
            Expr<T> default_cond = new Not<>(col_system.fold_or(conditions.values().stream().toList()), OriGen.create());
            conditions.put(default_case, default_cond);
        }

        // Then, create the body for each case expression
        for (java.util.Map.Entry<CaseExpression, Expr<T>> entry : conditions.entrySet()) {
            Expr<T> local_cond = entry.getValue();

            // Create body of case expression
            java.util.List<Statement<T>> block_body = new java.util.ArrayList<>();
            for (Expression expression : entry.getKey().getBody()) {
                // TODO: Handle break expressions in switch cases! Currently, we assume that there will be exactly one
                //       break statement at the end and only the end of each case. This could be a wrong assumption if
                //       there is a break statement elsewhere in the case.
                if (!(expression instanceof BreakExpression)) {
                    Statement<T> statement = create_statement(expression, sc_inst, obj, new And<>(path_cond, local_cond, OriGen.create()));
                    if (statement != null) {
                        block_body.add(statement);
                        local_cond = col_system.TRUE;
                    }
                }
            }

            // Add body as branch to the list of branches
            if (!block_body.isEmpty()) {
                Statement<T> body = new Block<>(List.from(CollectionConverters.asScala(block_body)), OriGen.create());
                branches.add(new Tuple2<>(entry.getValue(), body));
            }
        }

        // If there are no viable branches, ignore this switch statement
        if (branches.isEmpty()) return null;

        // Return the branches
        Statement<T> result = new Branch<>(List.from(CollectionConverters.asScala(branches)), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a SystemC while loop to a COL loop.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj Object representing the current access depth up to and including <code>sc_inst</code>
     * @param path_cond Path condition at this statement (only used for the first expression of a block)
     * @return A COL loop encoding the semantics of the given while loop
     */
    private Statement<T> transform_while_loop_expression(WhileLoopExpression expr, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);

        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        LoopContract<T> contract = specification_transformer.create_loop_invariant(path_cond);

        Block<T> body = expression_list_to_block(expr.getLoopBody(), sc_inst, obj, new And<>(path_cond, cond, OriGen.create()));
        Statement<T> result = new Loop<>(col_system.get_empty_block(), cond, col_system.get_empty_block(), contract, body, OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    // ============================================================================================================== //
    // ============================================== HELPER FUNCTIONS ============================================== //
    // ============================================================================================================== //

    /**
     * Helper function that transforms a function call expression with the <code>wait</code> function into the COL
     * encoding of such a wait, consisting of updates to the process and event states, followed by a loop that releases
     * and reacquires the global lock until the process is again ready to execute.
     *
     * @param expr Function call expression with a call to the <code>wait</code> function
     * @param sc_inst SystemC class instance this expression is referring to
     * @param obj COL expression encoding the accesses to this point
     * @return A block of statements encoding the wait expression
     */
    private Statement<T> transform_wait_expression(FunctionCallExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        java.util.List<Statement<T>> statements = new java.util.ArrayList<>();

        // Process and event field refs
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

        Ref<T, InstanceField<T>> proc_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Ref<T, InstanceField<T>> event_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> procs_deref = new Deref<>(m_deref, proc_ref, new GeneratedBlame<>(), OriGen.create());
        Deref<T> events_deref = new Deref<>(m_deref, event_ref, new GeneratedBlame<>(), OriGen.create());

        // Get parameters of wait call
        java.util.List<Expression> params = expr.getParameters();

        // Find ID of process
        int process_id = corr_proc.get_process_id();
        IntegerValue<T> proc_id = new IntegerValue<>(BigInt.apply(process_id), OriGen.create());

        // Find ID of event
        int event_id;
        switch (params.size()) {
            case 1 -> {
                SCEvent event_var = (SCEvent) ((SCVariableExpression) expr.getParameters().get(0)).getVar();
                event_id = col_system.get_shared_event(sc_inst, event_var);
            }
            case 2 -> {
                event_id = col_system.get_total_nr_events();
                col_system.add_wait_event();
            }
            default -> throw new UnsupportedException("Static sensitivity is not yet supported!", expr); // TODO: Support static sensitivity
        }
        IntegerValue<T> ev_id = new IntegerValue<>(BigInt.apply(event_id), OriGen.create());

        // Create process state update
        SeqUpdate<T> update_proc = new SeqUpdate<>(procs_deref, proc_id, ev_id, OriGen.create());
        statements.add(new Assign<>(procs_deref, update_proc, new GeneratedBlame<>(), OriGen.create()));

        // If the wait is waiting for time, also notify the event
        if (params.size() == 2) {
            Expression time_units = params.get(0);
            TimeUnitExpression unit = (TimeUnitExpression) params.get(1);

            // Find expression for the wait time
            Expr<T> wait_time;
            if (time_units instanceof ConstantExpression nr_units) {
                // Wait time can just be converted into literal
                long total_wait_time = Timing.getTimeUnits(nr_units.getValue(), unit.getTimeUnit().name());
                if (total_wait_time == 0) wait_time = col_system.MINUS_ONE;
                else wait_time = new IntegerValue<>(BigInt.apply(total_wait_time), OriGen.create());
            }
            else {
                // Get the number of units and the conversion factor
                Expr<T> units = create_expression(time_units, sc_inst, obj);
                long conversion_factor = Math.round(Timing.getTimeFactor(unit.getTimeUnit().name()));

                // Get an expression for the total number of time resolution time units
                Expr<T> total_nr_units;
                if (conversion_factor == 1) total_nr_units = units;
                else total_nr_units = new Mult<>(new IntegerValue<>(BigInt.apply(conversion_factor), OriGen.create()), units, OriGen.create());

                // Create selection of the form (var == 0 ? -1 : var * factor) to encode delta-delayed wait if var is 0
                Eq<T> zero_units = new Eq<>(units, col_system.ZERO, OriGen.create());
                wait_time = new Select<>(zero_units, col_system.MINUS_ONE, total_nr_units, OriGen.create());
            }

            // Create event state update
            SeqUpdate<T> update_event = new SeqUpdate<>(events_deref, ev_id, wait_time, OriGen.create());
            statements.add(new Assign<>(events_deref, update_event, new GeneratedBlame<>(), OriGen.create()));
        }

        // Add wait loop and finish block
        statements.add(create_wait_loop(proc_id, ev_id));
        Statement<T> result = new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());

        // Handle label
        return append_label(result, expr);
    }

    /**
     * Transforms a call to a FIFO channel function (either read or write). Since the FIFO itself is implemented as an
     * abstract function without a body, it cannot wait for read or write events if the queue is full or empty,
     * respectively. Therefore, the calling method must take this into account. This method creates a wait loop, the
     * function invocation of the appropriate FIFO function and, if a variable is given to assign to, an assignment of
     * the result to the given variable (otherwise just the function call).
     *
     * @param fifo Expression representing the FIFO queue
     * @param fun Expression representing the function call to the FIFO queue
     * @param sc_inst The SystemC class instance these expressions refer to
     * @param obj COL expression encoding the accesses to this point
     * @param assign_to (Optional) The variable the result of the FIFO call should be written to
     * @param op String encoding the assignment operator
     * @return A block statement containing everything necessary for the FIFO call
     */
    private Statement<T> transform_fifo_call_expression(SCPortSCSocketExpression fifo, FunctionCallExpression fun, SCClassInstance sc_inst,
                                                        Expr<T> obj, Expr<T> assign_to, String op) {
        java.util.List<Statement<T>> all_stmts = new java.util.ArrayList<>();

        // Process field reference
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());
        Ref<T, InstanceField<T>> proc_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> procs_deref = new Deref<>(m_deref, proc_ref, new GeneratedBlame<>(), OriGen.create());

        // Decode the fifo queue
        SCPort sc_port = fifo.getSCPortSCSocket();
        SCKnownType channel = col_system.get_primitive_port_connection(sc_inst, sc_port);
        Expr<T> fifo_queue = transform_sc_port_sc_socket_expression(fifo, sc_inst);
        InstanceField<T> fifo_buffer = col_system.get_primitive_instance_field(channel, Constants.FIFO_BUFFER);
        Ref<T, InstanceField<T>> buf_ref = new DirectRef<>(fifo_buffer, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> buf_deref = new Deref<>(fifo_queue, buf_ref, new GeneratedBlame<>(), OriGen.create());
        Size<T> buf_size = new Size<>(buf_deref, OriGen.create());
        InstanceField<T> fifo_written = col_system.get_primitive_instance_field(channel, Constants.FIFO_WRITTEN);
        Ref<T, InstanceField<T>> written_ref = new DirectRef<>(fifo_written, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> written_deref = new Deref<>(fifo_queue, written_ref, new GeneratedBlame<>(), OriGen.create());
        Size<T> written_size = new Size<>(written_deref, OriGen.create());
        InstanceField<T> fifo_num_read = col_system.get_primitive_instance_field(channel, Constants.FIFO_NUM_READ);
        Ref<T, InstanceField<T>> read_ref = new DirectRef<>(fifo_num_read, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> read_deref = new Deref<>(fifo_queue, read_ref, new GeneratedBlame<>(), OriGen.create());

        // Get a reference to the FIFO size parameter
        Ref<T, InstanceField<T>> fifo_size_ref = new DirectRef<>(col_system.get_fifo_size_parameter(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> fifo_size_deref = new Deref<>(m_deref, fifo_size_ref, new GeneratedBlame<>(), OriGen.create());

        // Decode the function call
        Expr<T> cond;
        int method_index;
        int wait_event_index;
        switch (fun.getFunction().getName()) {
            case "write" -> {
                Plus<T> total_size = new Plus<>(buf_size, written_size, OriGen.create());
                cond = new GreaterEq<>(total_size, fifo_size_deref, OriGen.create());
                method_index = Constants.FIFO_WRITE_METHOD;
                wait_event_index = Constants.FIFO_READ_EVENT;
            }
            case "read" -> {
                cond = new LessEq<>(buf_size, read_deref, OriGen.create());
                method_index = Constants.FIFO_READ_METHOD;
                wait_event_index = Constants.FIFO_WRITE_EVENT;
            }
            default -> throw new UnsupportedException("FIFO method " + fun.getFunction().getName() + " is not supported.", fun);
        }
        InstanceMethod<T> fifo_method = col_system.get_primitive_instance_method(channel, method_index);
        Ref<T, InstanceMethod<T>> method_ref = new DirectRef<>(fifo_method, ClassTag$.MODULE$.apply(InstanceMethod.class));

        // Decode function call parameters
        java.util.List<Expr<T>> args = new java.util.ArrayList<>();
        for (Expression param : fun.getParameters()) {
            Expr<T> arg = create_expression(param, sc_inst, obj);
            if (arg == null) throw new ExpressionParseException("Function call parameter " + param + " could not be parsed!", param);
            args.add(arg);
        }

        // Create process and event id
        IntegerValue<T> proc_id = new IntegerValue<>(BigInt.apply(corr_proc.get_process_id()), OriGen.create());
        int event_id = col_system.get_channel_events(channel).get(wait_event_index);
        IntegerValue<T> ev_id = new IntegerValue<>(BigInt.apply(event_id), OriGen.create());

        // Create wait statement
        SeqUpdate<T> new_process_state = new SeqUpdate<>(procs_deref, proc_id, ev_id, OriGen.create());
        Assign<T> update_process_state = new Assign<>(procs_deref, new_process_state, new GeneratedBlame<>(), OriGen.create());

        // Create wait loop
        Loop<T> wait_loop = create_wait_loop(proc_id, ev_id);

        // Create loop invariant for the big loop
        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        LoopContract<T> loop_contract = specification_transformer.create_loop_invariant(col_system.TRUE);   // Path condition is not relevant for generated loop

        // Create big loop
        java.util.List<Statement<T>> statements = java.util.List.of(update_process_state, wait_loop);
        Block<T> loop_body = new Block<>(List.from(CollectionConverters.asScala(statements)), OriGen.create());
        all_stmts.add(new Loop<>(col_system.get_empty_block(), cond, col_system.get_empty_block(), loop_contract, loop_body, OriGen.create()));

        // Create final statement, depending on whether a variable to assign to is given
        if (assign_to == null) {
            all_stmts.add(new InvokeMethod<>(fifo_queue, method_ref, List.from(CollectionConverters.asScala(args)), col_system.NO_EXPRS,
                    col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create()));
        }
        else {
            MethodInvocation<T> method_invocation = new MethodInvocation<>(fifo_queue, method_ref, List.from(CollectionConverters.asScala(args)),
                    col_system.NO_EXPRS, col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
            all_stmts.add(decode_assignment(assign_to, op, method_invocation));
        }

        // Put it all together and return
        return new Block<>(List.from(CollectionConverters.asScala(all_stmts)), OriGen.create());
    }

    /**
     * Transforms a call to a signal channel function (either read or write). Appropriately transforms the method
     * invocation and, if <code>assign_to</code> is given, assigns its value to the given variable expression.
     *
     * @param signal Expression representing the signal channel
     * @param fun Expression representing the function call to the signal channel
     * @param sc_inst The SystemC class instance these expressions refer to
     * @param obj COL expression encoding the accesses to this point
     * @param assign_to (Optional) The variable the result of the FIFO call should be written to
     * @param op String encoding the assignment operator
     * @return If <code>assign_to</code> is given, an assignment of the function call result to the given variable
     *         expression, else a lone function invocation of the given signal channel function
     */
    private Statement<T> transform_signal_call_expression_to_statement(SCPortSCSocketExpression signal, FunctionCallExpression fun,
                                                                       SCClassInstance sc_inst, Expr<T> obj, Expr<T> assign_to, String op) {

        // Get signal instance
        SCPort sc_port = signal.getSCPortSCSocket();
        SCKnownType channel = col_system.get_primitive_port_connection(sc_inst, sc_port);
        Expr<T> sc_signal = transform_sc_port_sc_socket_expression(signal, sc_inst);

        // Get method
        SCFunction sc_fun = fun.getFunction();
        int method_index = switch (sc_fun.getName()) {
            case "write" -> Constants.SIGNAL_WRITE_METHOD;
            case "read" -> Constants.SIGNAL_READ_METHOD;
            default -> throw new UnsupportedException("Signal method " + sc_fun.getName() + " is not supported!", fun);
        };
        InstanceMethod<T> signal_method = col_system.get_primitive_instance_method(channel, method_index);
        Ref<T, InstanceMethod<T>> method_ref = new DirectRef<>(signal_method, ClassTag$.MODULE$.apply(InstanceMethod.class));

        // Decode function call parameters
        java.util.List<Expr<T>> args = new java.util.ArrayList<>();
        for (Expression param : fun.getParameters()) {
            Expr<T> arg = create_expression(param, sc_inst, obj);
            if (arg == null) throw new ExpressionParseException("Function call parameter " + param + " could not be parsed!", param);
            args.add(arg);
        }

        // Create final statement, depending on whether a variable to assign to is given
        if (assign_to == null) {
            return new InvokeMethod<>(sc_signal, method_ref, List.from(CollectionConverters.asScala(args)), col_system.NO_EXPRS,
                    col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
        }
        else {
            MethodInvocation<T> method_invocation = new MethodInvocation<>(sc_signal, method_ref, List.from(CollectionConverters.asScala(args)),
                    col_system.NO_EXPRS, col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
            return decode_assignment(assign_to, op, method_invocation);
        }
    }

    /**
     * Decodes an assignment operator and returns the correct assignment with the (COL-encoded) left and right sides of
     * the assignment and the assignment operator given.
     *
     * @param left Variable expression to be assigned to
     * @param op String representation of assignment operator
     * @param right Right-hand side expression of assignment
     * @return A COL assignment encoding the given assignment
     */
    private Assign<T> decode_assignment(Expr<T> left, String op, Expr<T> right) {
        Expr<T> assign_value = switch (op) {
            case "=" -> right;
            case "+=" -> new Plus<>(left, right, OriGen.create());
            case "-=" -> new Minus<>(left, right, OriGen.create());
            case "*=" -> new Mult<>(left, right, OriGen.create());
            case "/=" -> new Div<>(left, right, new GeneratedBlame<>(), OriGen.create());
            case "%=" -> new Mod<>(left, right, new GeneratedBlame<>(), OriGen.create());
            case "&=" -> new AmbiguousComputationalAnd<>(left, right, OriGen.create());
            case "|=" -> new AmbiguousComputationalOr<>(left, right, OriGen.create());
            case "^=" -> new BitXor<>(left, right, OriGen.create());
            case ">>=" -> new BitShr<>(left, right, OriGen.create());
            case "<<=" -> new BitShl<>(left, right, OriGen.create());
            default -> throw new IllegalOperationException("Trying to transform an expression to a statement!");
        };

        return new Assign<>(left, assign_value, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Helper function that creates a wait loop for the given process and event ID.
     *
     * @param process_id ID of the process that should wait
     * @param event_id ID of the event it should wait on
     * @return A loop that unlocks the global lock until the event occurred and the process is woken up
     */
    private Loop<T> create_wait_loop(IntegerValue<T> process_id, IntegerValue<T> event_id) {
        // Process and event field refs
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

        Ref<T, InstanceField<T>> proc_ref = new DirectRef<>(col_system.get_process_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Ref<T, InstanceField<T>> event_ref = new DirectRef<>(col_system.get_event_state(), ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> procs_deref = new Deref<>(m_deref, proc_ref, new GeneratedBlame<>(), OriGen.create());
        Deref<T> events_deref = new Deref<>(m_deref, event_ref, new GeneratedBlame<>(), OriGen.create());

        // Create waiting loop body
        Unlock<T> unlock_m = new Unlock<>(m_deref, new GeneratedBlame<>(), OriGen.create());
        Lock<T> lock_m = new Lock<>(m_deref, new GeneratedBlame<>(), OriGen.create());
        java.util.List<Statement<T>> loop_body_statements = java.util.List.of(unlock_m, lock_m);
        Block<T> loop_body = new Block<>(List.from(CollectionConverters.asScala(loop_body_statements)), OriGen.create());

        // Create loop condition
        SeqSubscript<T> proc_index = new SeqSubscript<>(procs_deref, process_id, new GeneratedBlame<>(), OriGen.create());
        Neq<T> proc_ready = new Neq<>(proc_index, col_system.MINUS_ONE, OriGen.create());
        SeqSubscript<T> ev_index = new SeqSubscript<>(events_deref, event_id, new GeneratedBlame<>(), OriGen.create());
        Neq<T> ev_notified = new Neq<>(ev_index, col_system.MINUS_TWO, OriGen.create());
        Or<T> loop_cond = new Or<>(proc_ready, ev_notified, OriGen.create());

        // Create loop contract
        SpecificationTransformer<T> specification_transformer = new SpecificationTransformer<>(col_class, col_system, m);
        LoopContract<T> loop_contract = specification_transformer.create_loop_invariant(col_system.TRUE);   // Path condition is not relevant for generated loop

        // Put it together and return the loop
        return new Loop<>(col_system.get_empty_block(), loop_cond, col_system.get_empty_block(), loop_contract, loop_body, OriGen.create());
    }

    /**
     * Creates a new randomizing function and returns a reference to it.
     *
     * @param return_type Return type of the random function
     * @return A reference to a new function for generating a random value
     */
    private Ref<T, InstanceMethod<T>> create_random_function(Type<T> return_type) {
        // Register new generated function in intermediate representation
        int nr_gen = col_class.get_generated_functions();
        String name = "generate__random__" + nr_gen;
        col_class.add_generated_functions(1);

        // Create completely empty method with appropriate return type
        ApplicableContract<T> contract = new ApplicableContract<>(new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()),
                new UnitAccountedPredicate<>(col_system.TRUE, OriGen.create()), col_system.TRUE, col_system.NO_SIGNALS,
                col_system.NO_VARS, col_system.NO_VARS, Option.empty(), new GeneratedBlame<>(), OriGen.create());
        InstanceMethod<T> randomizer = new InstanceMethod<>(return_type, col_system.NO_VARS, col_system.NO_VARS, col_system.NO_VARS,
                Option.empty(), contract, false, true, new GeneratedBlame<>(), OriGen.create(name));
        newly_generated_methods.add(randomizer);

        // Return reference to the new method
        return new DirectRef<>(randomizer, ClassTag$.MODULE$.apply(InstanceMethod.class));
    }

    /**
     * Helper function that converts a list of expressions to a block of COL statements. If none of the statements in
     * the block convert to anything other than null, then this function returns null as well.
     *
     * @param expressions A list of expressions to be converted
     * @param sc_inst SystemC class instance that these expressions refer to
     * @param obj COL expression encoding the accesses to this point
     * @param path_cond Path condition at this point (only used for the first statement of the block)
     * @return A block of all statements that can be converted, or null if no statements can be converted
     */
    private Block<T> expression_list_to_block(java.util.List<Expression> expressions, SCClassInstance sc_inst, Expr<T> obj, Expr<T> path_cond) {
        java.util.List<Statement<T>> block_body = new java.util.ArrayList<>();

        Expr<T> path_condition = path_cond;
        for (Expression expression : expressions) {
            Statement<T> statement = create_statement(expression, sc_inst, obj, path_condition);
            if (statement != null) {
                block_body.add(statement);
                path_condition = col_system.TRUE;
            }
        }

        if (block_body.isEmpty()) return null;
        return new Block<>(List.from(CollectionConverters.asScala(block_body)), OriGen.create());
    }

    /**
     * Creates a label statement and registers the label in the COL system if the given expression contains a label.
     *
     * @param expr SystemC expression
     * @return A COL Label statement if <code>expr</code> contains a label, null otherwise
     */
    private Label<T> handle_expression_label(Expression expr) {
        String expr_label = expr.getLabel();
        if (expr_label == null || expr_label.isEmpty()) return null;

        LabelDecl<T> label = new LabelDecl<>(OriGen.create(expr_label));
        col_system.add_label(expr_label, label);
        return new Label<>(label, col_system.get_empty_block(), OriGen.create(expr_label));
    }

    /**
     * Adds the label to the given statement by combining the label and the statement to a block statement.
     *
     * @param stmt COL statement
     * @param expr SystemC expression, possibly containing a label
     * @return A block of the form <code>(label, stmt)</code>
     */
    private Statement<T> append_label(Statement<T> stmt, Expression expr) {
        Label<T> label = handle_expression_label(expr);
        if (label == null) return stmt;
        if (stmt == null) return label;

        java.util.List<Statement<T>> results = java.util.List.of(label, stmt);
        return new Block<>(List.from(CollectionConverters.asScala(results)), OriGen.create());
    }

    // ============================================================================================================== //
    // ============================================================================================================== //
    // ============================================================================================================== //

    // ============================================================================================================== //
    // ============================ TRANSFORM FROM SYSTEMC EXPRESSION TO COL EXPRESSIONS ============================ //
    // ============================================================================================================== //

    /**
     * Transforms the given SystemC expression into a COL expression with no context. Throws a NullPointerException if
     * context is required to parse the expression, i.e. if the expression contains variables, ports or function calls.
     *
     * @param expr Expression to be transformed, consisting only of simple constants, enum values and operators
     * @return An equivalent expression in COL
     * @throws NullPointerException if the given expression contains subexpressions that require context to parse
     */
    public Expr<T> transform_simple_expression(Expression expr) throws NullPointerException {
        return create_expression(expr, null, col_system.THIS);      // TODO: There has to be a better way to encode this
    }

    /**
     * Transforms the given expression to a COL expression, given that it is operating on the class instance
     * <code>sc_inst</code> and got there through the accesses encoded in <code>obj</code>.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> create_expression(Expression expr, SCClassInstance sc_inst, Expr<T> obj) {
        if (expr == null) {
            return null;
        }
        if (expr instanceof AccessExpression e) {
            return transform_access_expression_to_expression(e, sc_inst, obj);
        }
        if (expr instanceof ArrayAccessExpression e) {
            return transform_array_access_expression(e, sc_inst, obj);
        }
        if (expr instanceof BinaryExpression e) {
            return transform_binary_expression_to_expression(e, sc_inst, obj);
        }
        if (expr instanceof BracketExpression e) {
            return transform_bracket_expression(e, sc_inst, obj);
        }
        if (expr instanceof ConstantExpression e) {
            return transform_constant_expression(e);
        }
        if (expr instanceof EmptyExpression) {
            return null;
        }
        if (expr instanceof EnumElementExpression e) {
            return transform_enum_element_expression(e);
        }
        if (expr instanceof FunctionCallExpression e) {
            pure = false;
            return transform_function_call_expression_to_expression(e, sc_inst, obj);
        }
        if (expr instanceof NewArrayExpression e) {
            pure = false;
            return transform_new_array_expression(e, sc_inst, obj);
        }
        if (expr instanceof QuestionmarkExpression e) {
            return transform_question_mark_expression(e, sc_inst, obj);
        }
        if (expr instanceof RefDerefExpression e) {
            return transform_ref_deref_expression(e, sc_inst, obj);
        }
        if (expr instanceof SCPortSCSocketExpression e) {
            return transform_sc_port_sc_socket_expression(e, sc_inst);
        }
        if (expr instanceof SCVariableExpression e) {
            return transform_sc_variable_expression(e, sc_inst, obj);
        }
        if (expr instanceof UnaryExpression e) {
            return transform_unary_expression(e, sc_inst, obj);
        }
        // TODO: ArrayInitializerExpression, NewExpression, SCClassInstanceExpression
        throw new ExpressionParseException("The following expression is not supported:\n\n" + expr, expr);
    }

    /**
     * Transforms an access expression to a COL expression.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_access_expression_to_expression(AccessExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        SCClassInstance next_inst = sc_inst;
        if (expr.getLeft() instanceof SCClassInstanceExpression cls_inst_expr) {
            next_inst = cls_inst_expr.getInstance();
        }
        if (expr.getLeft() instanceof SCPortSCSocketExpression sc_port_expr) {
            SCPort sc_port = sc_port_expr.getSCPortSCSocket();
            SCClassInstance connected_channel = col_system.get_hierarchical_port_connection(sc_inst, sc_port);
            if (connected_channel != null) {
                next_inst = connected_channel;
            }
        }
        Expr<T> left = create_expression(expr.getLeft(), sc_inst, obj);
        return create_expression(expr.getRight(), next_inst, left);
    }

    /**
     * Transforms an array access to COL.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_array_access_expression(ArrayAccessExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        SCVariable array = expr.getVar();
        Ref<T, InstanceField<T>> var_ref = new LazyRef<>(() -> col_system.get_instance_field(sc_inst, array), Option.empty(),
                ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> var_deref = new Deref<>(col_system.THIS, var_ref, new GeneratedBlame<>(), OriGen.create());

        // Get index    TODO: What about multidimensional arrays?
        Expr<T> index = create_expression(expr.getAccess().get(0), sc_inst, obj);

        return new ArraySubscript<>(var_deref, index, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Transforms a binary expression to an equivalent expression in COL.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_binary_expression_to_expression(BinaryExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        if (expr.getOp().equals("=")) throw new IllegalOperationException("Trying to convert a statement to an expression!");

        // Disallow FIFO calls in binary expressions
        if (   (expr.getLeft() instanceof AccessExpression acc_l && acc_l.getLeft() instanceof SCPortSCSocketExpression sc_port_l
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port_l.getSCPortSCSocket().getConType()))
            || (expr.getRight() instanceof AccessExpression acc_r && acc_r.getLeft() instanceof SCPortSCSocketExpression sc_port_r
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port_r.getSCPortSCSocket().getConType()))) {
            throw new UnsupportedException("FIFO calls in binary expressions are not allowed!", expr);
        }

        // Get left side of binary expression
        Expr<T> left;
        // Channel expression (only signal allowed) needs to be handled separately
        if (expr.getLeft() instanceof AccessExpression acc_l && acc_l.getLeft() instanceof SCPortSCSocketExpression sc_port_l
                && acc_l.getRight() instanceof FunctionCallExpression fun
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port_l.getSCPortSCSocket().getConType())) {
            left = transform_signal_call_expression_to_expression(sc_port_l, fun, sc_inst);
        }
        // Otherwise recursively decode expression
        else {
            left = create_expression(expr.getLeft(), sc_inst, obj);
        }

        // Get right side of binary expression
        Expr<T> right;
        // Channel expression (only signal allowed) needs to be handled separately
        if (expr.getRight() instanceof AccessExpression acc_r && acc_r.getLeft() instanceof SCPortSCSocketExpression sc_port_r
                && acc_r.getRight() instanceof FunctionCallExpression fun
                && SCPORTSCSOCKETTYPE.SC_FIFO_ALL.contains(sc_port_r.getSCPortSCSocket().getConType())) {
            right = transform_signal_call_expression_to_expression(sc_port_r, fun, sc_inst);
        }
        // Otherwise recursively decode expression
        else {
            right = create_expression(expr.getRight(), sc_inst, obj);
        }

        if (left == null || right == null) throw new ExpressionParseException("Cannot convert binary expression operands in " + expr, expr);

        // TODO: Are any binary operators missing?
        return switch (expr.getOp()) {
            case "+" -> new Plus<>(left, right, OriGen.create());
            case "-" -> new Minus<>(left, right, OriGen.create());
            case "*" -> new Mult<>(left, right, OriGen.create());
            case "/" -> new FloorDiv<>(left, right, new GeneratedBlame<>(), OriGen.create());
            case "%" -> new Mod<>(left, right, new GeneratedBlame<>(), OriGen.create());
            case "==" -> new Eq<>(left, right, OriGen.create());
            case "!=" -> new Neq<>(left, right, OriGen.create());
            case ">" -> new Greater<>(left, right, OriGen.create());
            case "<" -> new Less<>(left, right, OriGen.create());
            case ">=" -> new GreaterEq<>(left, right, OriGen.create());
            case "<=" -> new LessEq<>(left, right, OriGen.create());
            case "&&" -> new And<>(left, right, OriGen.create());
            case "||" -> new Or<>(left, right, OriGen.create());
            case "&" -> new AmbiguousComputationalAnd<>(left, right, OriGen.create());
            case "|" -> new AmbiguousComputationalOr<>(left, right, OriGen.create());
            case "^" -> new BitXor<>(left, right, OriGen.create());
            case ">>" -> new BitShr<>(left, right, OriGen.create());
            case "<<" -> new BitShl<>(left, right, OriGen.create());
            default -> throw new UnsupportedException("Unsupported binary operator " + expr.getOp(), expr);
        };
    }

    /**
     * Transforms a bracket expression; simply ignores the brackets and translates the internal expression.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_bracket_expression(BracketExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        return create_expression(expr.getInBrackets(), sc_inst, obj);
    }

    /**
     * Parses a SystemC constant. If the constant is empty, it is ignored. Otherwise, it is converted to either a
     * boolean or an integer.
     *
     * @param expr Expression to be converted
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_constant_expression(ConstantExpression expr) {
        String literal = expr.getValue();

        if (literal.isEmpty()) return null;

        // Try parsing integer
        try {
            long res = Long.parseLong(literal);
            return new IntegerValue<>(BigInt.apply(res), OriGen.create());
        }
        catch (NumberFormatException ignored) {}

        // Try parsing boolean
        if (literal.equals("true")) return col_system.TRUE;
        if (literal.equals("false")) return col_system.FALSE;

        // Unsupported literal type
        throw new ExpressionParseException("Expression " + literal + " is of unsupported type!", expr);
    }

    /**
     * Converts an enum element into the integer it represents.
     *
     * @param expr Expression to be converted
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_enum_element_expression(EnumElementExpression expr) {
        // Enum elements are transformed to integers in the encoding
        return new IntegerValue<>(BigInt.apply(expr.getEnumElement().getValue()), OriGen.create());
    }

    /**
     * Transforms a SystemC function call into a COL expression. Either searches for the method to call in the COL
     * system context or, if it is an internal function call (e.g. random), generates a new method and calls that.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_function_call_expression_to_expression(FunctionCallExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        // Find appropriate COL function (or reference thereto)
        SCFunction sc_fun = expr.getFunction();
        // If the function is a random function, generate a new one
        Ref<T, InstanceMethod<T>> col_fun = switch (sc_fun.getName()) {
            case "rand", "rand_r", "random" -> create_random_function(col_system.T_INT);
            case "srand", "sc_module" -> null;
            default -> new LazyRef<>(() -> col_system.get_instance_method(sc_fun, sc_inst, corr_proc), Option.empty(),
                    ClassTag$.MODULE$.apply(InstanceMethod.class));
        };

        // Ignore the srand function and the sc_module constructor call
        if (col_fun == null) return null;

        // Transform parameter list
        java.util.List<Expr<T>> arguments = new java.util.ArrayList<>();
        for (Expression param : expr.getParameters()) {
            Expr<T> parameter = create_expression(param, sc_inst, obj);
            if (parameter == null) throw new ExpressionParseException("Function call parameter " + param + " could not be parsed!", param);
            arguments.add(parameter);
        }

        // Finish the method invocation
        return new MethodInvocation<>(obj, col_fun, List.from(CollectionConverters.asScala(arguments)), col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Transforms an array initialization to COL.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_new_array_expression(NewArrayExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Type<T> array_type = col_system.parse_type(expr.getObjType());  // TODO: What about multi-dimensional arrays?
        Expr<T> size = create_expression(expr.getSize(), sc_inst, obj);

        if (size == null) return new NewArray<>(array_type, col_system.NO_EXPRS, 1, OriGen.create());
        else return new NewArray<>(array_type, List.from(CollectionConverters.asScala(java.util.List.of(size))), 0, OriGen.create());
    }

    /**
     * Transforms a SystemC selection expression (<code>cond ? v1 : v2</code>) to a COL selection.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_question_mark_expression(QuestionmarkExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Expr<T> cond = create_expression(expr.getCondition(), sc_inst, obj);
        Expr<T> then_val = create_expression(expr.getThen(), sc_inst, obj);
        Expr<T> else_val = create_expression(expr.getElse(), sc_inst, obj);
        if (cond == null || then_val == null || else_val == null) throw new ExpressionParseException("Conditional selection could not be transformed!", expr);

        return new Select<>(cond, then_val, else_val, OriGen.create());
    }

    /**
     * Transforms a pointer dereferencing/referencing expression into COL. Since pointers are not yet supported, this
     * just handles the referenced/dereferenced expression as if it was a regular expression.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_ref_deref_expression(RefDerefExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        return create_expression(expr.getExpression(), sc_inst, obj);     // TODO: Should we do something else with pointers?
    }

    /**
     * Transforms a SystemC port/socket into the corresponding COL field.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_sc_port_sc_socket_expression(SCPortSCSocketExpression expr, SCClassInstance sc_inst) {
        // Main reference field
        Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
        Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

        // Decode the port/socket expression
        SCPort sc_port = expr.getSCPortSCSocket();
        SCKnownType prim_channel = col_system.get_primitive_port_connection(sc_inst, sc_port);
        SCClassInstance hier_channel = col_system.get_hierarchical_port_connection(sc_inst, sc_port);

        // Find the corresponding field in the Main class
        Ref<T, InstanceField<T>> channel_ref;
        if (prim_channel != null) {
            channel_ref = new DirectRef<>(col_system.get_primitive_channel(prim_channel), ClassTag$.MODULE$.apply(InstanceField.class));
        }
        else if (hier_channel != null) {
            channel_ref = new LazyRef<>(() -> col_system.get_instance_by_class(col_system.get_state_class(hier_channel)), Option.empty(),
                    ClassTag$.MODULE$.apply(InstanceField.class));
        }
        else throw new SystemCFormatException("SCPortSCSocketExpression " + expr + " does not have any connected channel!");

        // Return a reference to the field
        return new Deref<>(m_deref, channel_ref, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Transforms a SystemC variable expression into the corresponding COL field dereference. Finds the variable in the
     * COL system and, if necessary, the object the variable is an instance of.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_sc_variable_expression(SCVariableExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        SCVariable sc_var = expr.getVar();

        // If the variable is local, return the local COL equivalent
        if (local_variables.containsKey(sc_var)) {
            Variable<T> var = local_variables.get(sc_var);
            return new Local<>(new DirectRef<>(var, ClassTag$.MODULE$.apply(Variable.class)), var.o());
        }

        // If the variable is a parameter, return a reference of the Main class field
        if (col_system.is_parameter(sc_var)) {
            Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());
            InstanceField<T> param = col_system.get_parameter(sc_var);
            Ref<T, InstanceField<T>> param_ref = new DirectRef<>(param, ClassTag$.MODULE$.apply(InstanceField.class));
            return new Deref<>(m_deref, param_ref, new GeneratedBlame<>(), OriGen.create());
        }

        // Else find it in the global system
        VariableTransformer<T> variable_transformer = new VariableTransformer<>(sc_inst, col_system);
        InstanceField<T> var_field = variable_transformer.transform_variable_to_instance_field(sc_var);
        Ref<T, InstanceField<T>> var_ref = new DirectRef<>(var_field, ClassTag$.MODULE$.apply(InstanceField.class));

        // If the variable is an attribute of this class, but not of the corresponding COL class, access it through the containing instance
        // This works because the containing class can only be the state class or this one
        COLClass containing_class = col_system.get_containing_class(var_field);
        if (obj == col_system.THIS && !containing_class.equals(col_class)) {
            // Create m reference
            Ref<T, InstanceField<T>> m_ref = new DirectRef<>(m, ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> m_deref = new Deref<>(col_system.THIS, m_ref, new GeneratedBlame<>(), OriGen.create());

            // Create reference to the instance of the class containing the variable
            Ref<T, InstanceField<T>> containing_instance = new LazyRef<>(() -> col_system.get_instance_by_class(containing_class),
                    Option.empty(), ClassTag$.MODULE$.apply(InstanceField.class));
            Deref<T> containing_deref = new Deref<>(m_deref, containing_instance, new GeneratedBlame<>(), OriGen.create());

            // Return the complete path to the variable
            return new Deref<>(containing_deref, var_ref, new GeneratedBlame<>(), OriGen.create());
        }
        // Else return it in relation to the given access object
        return new Deref<>(obj, var_ref, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Transforms a unary SystemC operation to COL.
     *
     * @param expr Expression to be converted
     * @param sc_inst SystemC class instance that this expression refers to
     * @param obj COL expression encoding the accesses to this point
     * @return An expression encoding the semantics of the SystemC expression
     */
    private Expr<T> transform_unary_expression(UnaryExpression expr, SCClassInstance sc_inst, Expr<T> obj) {
        Expr<T> original = create_expression(expr.getExpression(), sc_inst, obj);
        if (original == null) return null;

        // TODO: Are any unary operators missing?
        return switch (expr.getOperator()) {
            case "!" -> new Not<>(original, OriGen.create());
            case "-" -> new UMinus<>(original, OriGen.create());
            case "~" -> new BitNot<>(original, OriGen.create());
            case "+" -> original;
            case "++" -> handle_incr_decr(true, expr.isPrepost(), original);
            case "--" -> handle_incr_decr(false, expr.isPrepost(), original);
            default -> throw new UnsupportedException("Unsupported unary operator " + expr.getOperator(), expr);
        };
    }

    // ============================================================================================================== //
    // ============================================== HELPER FUNCTIONS ============================================== //
    // ============================================================================================================== //

    /**
     * Generates pre- and postfix increment and decrement operators in COL.
     *
     * @param incr <code>true</code> if an increment operator should be generated, <code>false</code> for a decrement
     *             operator
     * @param pre <code>true</code> if the operator should be prefix, <code>false</code> for postfix
     * @param original Variable expression in COL to be assigned to
     * @return Depending on the given flags, a pre- or postfix increment or decrement operator on the given variable
     *         expression
     */
    private Expr<T> handle_incr_decr(boolean incr, boolean pre, Expr<T> original) {
        // Create increment/decrement expression
        Expr<T> incr_decr;
        if (incr) incr_decr = new Plus<>(original, new IntegerValue<>(BigInt.apply(1), OriGen.create()), OriGen.create());
        else incr_decr = new Minus<>(original, new IntegerValue<>(BigInt.apply(1), OriGen.create()), OriGen.create());

        if (pre) return new PreAssignExpression<>(original, incr_decr, new GeneratedBlame<>(), OriGen.create());
        else return new PostAssignExpression<>(original, incr_decr, new GeneratedBlame<>(), OriGen.create());
    }

    /**
     * Transforms a signal read function call to an expression that can be used, for instance, in a binary expression.
     *
     * @param signal Expression representing the signal channel
     * @param fun Expression representing the function call to the signal channel
     * @param sc_inst The SystemC class instance these expressions refer to
     * @return Method invocation expression of the signal channel
     */
    private Expr<T> transform_signal_call_expression_to_expression(SCPortSCSocketExpression signal, FunctionCallExpression fun,
                                                                   SCClassInstance sc_inst) {

        // Get signal instance
        SCPort sc_port = signal.getSCPortSCSocket();
        SCKnownType channel = col_system.get_primitive_port_connection(sc_inst, sc_port);
        Expr<T> sc_signal = transform_sc_port_sc_socket_expression(signal, sc_inst);

        // Get method
        if (!fun.getFunction().getName().equals("read")) {
            throw new UnsupportedException("Only read method calls are supported for signal channels in expressions.", fun);
        }
        InstanceMethod<T> signal_method = col_system.get_primitive_instance_method(channel, Constants.SIGNAL_READ_METHOD);
        Ref<T, InstanceMethod<T>> method_ref = new DirectRef<>(signal_method, ClassTag$.MODULE$.apply(InstanceMethod.class));

        // The signal read method has no parameters
        java.util.List<Expr<T>> args = java.util.List.of();

        // Return the method invocation
        return new MethodInvocation<>(sc_signal, method_ref, List.from(CollectionConverters.asScala(args)), col_system.NO_EXPRS,
                col_system.NO_TYPES, col_system.NO_GIVEN, col_system.NO_YIELDS, new GeneratedBlame<>(), OriGen.create());
    }

    // ============================================================================================================== //
    // ============================================================================================================== //
    // ============================================================================================================== //

    /**
     * Returns the methods that were generated in expression transformations so far.
     *
     * @return A list of newly generated methods
     */
    public java.util.List<InstanceMethod<T>> get_generated_methods() {
        return newly_generated_methods;
    }

    /**
     * Returns a boolean indicating whether the method containing the transformed expressions could be pure or not.
     *
     * @return <code>false</code> if any expression transformed could have side effects outside the containing method,
     *         and <code>true</code> otherwise
     */
    public boolean is_pure() {
        return pure;
    }
}

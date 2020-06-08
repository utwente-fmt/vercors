package vct.col.rewrite;

import hre.ast.BranchOrigin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.IntegerValue;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.NameScanner;
import vct.col.ast.stmt.decl.*;
import vct.col.util.OriginWrapper;
import vct.logging.ErrorMapping;
import vct.logging.VerCorsError.ErrorCode;

public class ParallelBlockEncoder extends AbstractRewriter {
    private class SendRecvInfo {
        final ArrayList<ASTNode> guards = new ArrayList<ASTNode>(guard_stack);

        final ASTNode stat;

        public SendRecvInfo(ASTNode stat) {
            this.stat = stat;
        }
    }

    public static final String ENTER_INVARIANT = "enter_inv";
    public static final String LEAVE_ATOMIC = "leave_atomic";

    public ParallelBlockEncoder(ProgramUnit source, ErrorMapping map) {
        super(source);
        map.add(ENTER_INVARIANT,
                ErrorCode.ExhaleFailed,
                ErrorCode.InvariantNotEstablished);
        map.add(LEAVE_ATOMIC,
                ErrorCode.ExhaleFailed,
                ErrorCode.InvariantBroken);
    }

    /**
     * Makes generated names unique
     */
    private int count = 0;

    /**
     * Makes some different names unique
     */
    private AtomicInteger counter = new AtomicInteger();

    /**
     * Stack of encountered ParallelInvariant, to scope the name of the invariant
     */
    private Stack<ASTNode> inv_blocks = new Stack<ASTNode>();

    /**
     * Scopes labeled ParallelBlocks for barriers with a specific par label
     */
    private Stack<ParallelBlock> blocks = new Stack<ParallelBlock>();


    /**
     * Contains the label of the current node or null. Used to ensure send/recv statements are labeled.
     */
    private String current_label;

    /**
     * Accumulated conditions from if(..) statements, used to guard send/recv statements.
     * Pretty sure this is incorrect for else if(..) statements, where earlier conditions must necessarily be false.
     */
    private Stack<ASTNode> guard_stack = new Stack<ASTNode>();

    /**
     * last encountered invariant from the contract of a parallel block or for-each loop, or null.
     */
    private ASTNode loop_invariant;

    private HashMap<String, SendRecvInfo> send_recv_map = new HashMap<String, SendRecvInfo>();

    @Override
    public void enter(ASTNode node) {
        super.enter(node);
        if (node.labels() > 0) {
            current_label = node.getLabel(0).getName();
            Debug("current label is %s", current_label);
        }
    }

    @Override
    public void leave(ASTNode node) {
        if (node.labels() > 0) {
            current_label = null;
        }
        super.leave(node);
    }

    /**
     * inv(E) { S }
     * ==> exhale E; S; inhale E;
     * with invariant on inv_blocks
     */
    @Override
    public void visit(ParallelInvariant inv) {
        inv_blocks.push(inv);
        BlockStatement block = rewrite(inv.block());

        ASTNode exhale = create.special(ASTSpecial.Kind.Exhale, rewrite(inv.inv()));
        exhale.set_branch(ENTER_INVARIANT);
        block.prepend(exhale);

        block.append(create.special(ASTSpecial.Kind.Inhale, rewrite(inv.inv())));

        result = block;
        inv_blocks.pop();
    }

    /**
     * ?? distinguish on empty/absent contract
     */
    @Override
    public void visit(ParallelRegion region) {
        count++;
        String main_name = "parrallel_region_main_" + count;
        ContractBuilder main_cb = new ContractBuilder();
        Hashtable<String, Type> main_vars = free_vars(region.blocksJava());
        BlockStatement body;
        if (region.contract() == null || region.contract().isEmpty()) {
            for (ParallelBlock pb : region.blocksJava()) {
                Contract c = (Contract) rewrite((ASTNode) pb);
                if (c != null) {
                    main_cb.requires(c.invariant);
                    main_cb.requires(c.pre_condition);
                    main_cb.ensures(c.post_condition);
                } else {
                    main_cb.requires(create.constant(true));
                    main_cb.ensures(create.constant(true));
                }
            }
            body = null;
        } else {
            rewrite(region.contract(), main_cb);
            body = create.block();
            for (ParallelBlock pb : region.blocksJava()) {
                String block_name = "block_check_" + (++count);
                Hashtable<String, Type> block_vars = free_vars(pb);

                Contract c = (Contract) rewrite((ASTNode) pb);
                currentTargetClass.add(create.method_decl(
                        create.primitive_type(PrimitiveSort.Void),
                        c,
                        block_name,
                        gen_pars(block_vars),
                        null
                ));
                body.add(gen_call(block_name, block_vars));
            }
            HashMap<String, ParallelBlock> blocks = new HashMap<String, ParallelBlock>();
            HashMap<String, HashSet<String>> may_deps = new HashMap<String, HashSet<String>>();
            HashMap<String, HashSet<String>> must_deps = new HashMap<String, HashSet<String>>();
            for (ParallelBlock pb : region.blocksJava()) {
                /* before is a set of blocks that are guaranteed
                 * not to run concurrently with the current block.
                 */
                HashSet<String> may = new HashSet<String>();
                may.add(pb.label());
                HashSet<String> must = new HashSet<String>();
                must.add(pb.label());
                for (int i = 0; i < pb.depslength(); i++) {
                    ASTNode d = pb.dependency(i);
                    if (d instanceof NameExpression) {
                        String dep = d.toString();
                        HashSet<String> trans = may_deps.get(dep);
                        if (trans == null) {
                            Fail("dependency %s of %s is unknown", dep, pb.label());
                        }
                        may.addAll(trans);
                        must.addAll(must_deps.get(dep));
                        ParallelBlock pb2 = blocks.get(dep);
                        ASTNode args[] = new ASTNode[pb2.iterslength()];
                        for (int j = 0; j < args.length; j++) {
                            args[j] = create.reserved_name(ASTReserved.Any);
                        }
                        pb.dependency(i, create.invokation(null, null, dep, args));
                    } else if (d instanceof MethodInvokation) {
                        MethodInvokation e = (MethodInvokation) d;
                        String dep = e.method;
                        HashSet<String> trans = must_deps.get(dep);
                        if (trans == null) {
                            Fail("dependency %s of %s is unknown", dep, pb.label());
                        }
                        boolean add = true;
                        for (ASTNode a : e.getArgs()) {
                            if (!a.isReserved(ASTReserved.Any)) {
                                add = false;
                                break;
                            }
                        }
                        if (add) {
                            must.addAll(trans);
                        }
                        may.addAll(may_deps.get(dep));
                    } else {
                        Fail("cannot deal with dependency %s", d);
                    }
                }
                for (String d : must_deps.keySet()) {
                    if (!must.contains(d)) {
                        gen_consistent(region, blocks.get(d), pb, may.contains(d));
                        if (may.contains(d)) {
                            ParallelBlock pb2 = blocks.get(d);
                            ArrayList<ASTNode> conds = new ArrayList<ASTNode>();
                            ArrayList<DeclarationStatement> decls = new ArrayList<DeclarationStatement>();

                            for (DeclarationStatement decl : pb2.itersJava()) {
                                decls.add(create.field_decl("x_" + decl.name(), decl.getType()));
                            }

                            HashMap<NameExpression, ASTNode> map = new HashMap<NameExpression, ASTNode>();
                            Substitution sigma = new Substitution(source(), map);

                            for (DeclarationStatement decl : pb.itersJava()) {
                                decls.add(create.field_decl("y_" + decl.name(), decl.getType()));
                                map.put(create.unresolved_name(decl.name()), create.unresolved_name("y_" + decl.name()));
                            }

                            for (int i = 0; i < pb.depslength(); i++) {
                                ASTNode dep_tmp = pb.dependency(i);
                                MethodInvokation dep = (MethodInvokation) dep_tmp;
                                String dname = dep.method;
                                if (pb2.label().equals(dname)) {
                                    ArrayList<ASTNode> parts = new ArrayList<ASTNode>();

                                    int j = 0;
                                    for (DeclarationStatement decl : pb2.itersJava()) {
                                        if (!dep.getArg(j).isReserved(ASTReserved.Any)) {
                                            parts.add(create.expression(StandardOperator.EQ,
                                                    create.argument_name("x_" + decl.name()),
                                                    sigma.rewrite(dep.getArg(j))
                                            ));
                                        }
                                        j++;
                                    }

                                    conds.add(create.fold(StandardOperator.And, parts));
                                } else {
                                    ParallelBlock pb1 = blocks.get(dname);
                                    if (must_deps.get(dname).contains(pb2.label())) {
                                        conds.add(create.constant(true));
                                        break;
                                    }
                                    ArrayList<DeclarationStatement> exists = new ArrayList<DeclarationStatement>();

                                    int pb1_iterslength = pb1.iterslength();
                                    int pb2_iterslength = pb2.iterslength();
                                    ASTNode args[] = new ASTNode[pb2_iterslength + pb1_iterslength];

                                    int j = 0;
                                    for (DeclarationStatement decl : pb2.itersJava()) {
                                        args[j] = create.argument_name("x_" + decl.name());
                                        j++;
                                    }

                                    for (j = 0; j < pb1_iterslength; j++) {
                                        if (dep.getArg(j).isReserved(ASTReserved.Any)) {
                                            args[pb2_iterslength + j] = create.unresolved_name("z_" + j);
                                        } else {
                                            args[pb2_iterslength + j] = sigma.rewrite(dep.getArg(j));
                                        }
                                    }

                                    ASTNode cond = create.invokation(null, null, "before_" + pb2.label() + "_" + pb1.label(), args);
                                    if (exists.size() > 0) {
                                        cond = create.exists(create.constant(true), cond, exists.toArray(new DeclarationStatement[0]));
                                    }
                                    conds.add(cond);
                                }
                            }
                            ASTNode cond = create.fold(StandardOperator.Or, conds);
                            currentTargetClass.add(create.function_decl(
                                    create.primitive_type(PrimitiveSort.Boolean),
                                    null,
                                    "before_" + pb2.label() + "_" + pb.label(),
                                    decls.toArray(new DeclarationStatement[0]),
                                    cond
                            ));
                        }
                    }
                }
                may_deps.put(pb.label(), may);
                must_deps.put(pb.label(), must);
                blocks.put(pb.label(), pb);
            }
        }
        currentTargetClass.add(create.method_decl(
                create.primitive_type(PrimitiveSort.Void),
                main_cb.getContract(),
                main_name,
                gen_pars(main_vars),
                body
        ));
        result = gen_call(main_name, main_vars);
    }

    /**
     * contract par(i=0..n) { S }
     * ==> ForEachLoop(contract, guard=i \member {0..n}) { S }
     * pass to do_block
     * with block on blocks
     */
    @Override
    public void visit(ParallelBlock pb) {
        Contract c = pb.contract();
        if (c == null) {
            Fail("parallel block without a contract");
        }
        blocks.push(pb);

        DeclarationStatement iters[] = pb.itersJava().toArray(new DeclarationStatement[0]);
        DeclarationStatement iter_decls[] = new DeclarationStatement[iters.length];
        ArrayList<ASTNode> guard_list = new ArrayList<ASTNode>();
        for (int i = 0; i < iter_decls.length; i++) {
            iter_decls[i] = create.field_decl(iters[i].name(), iters[i].getType());
            //iter_decls_prime[i]=create.field_decl(pb.iters[i].name+"__prime", pb.iters[i].getType());
            ASTNode member = create.local_name(iters[i].name());
            member.setType(iters[i].getType());
            ASTNode tmp = create.expression(StandardOperator.Member, member, iters[i].initJava());
            guard_list.add(tmp);
        }
        ASTNode iters_guard = create.fold(StandardOperator.And, guard_list);

        ASTNode res = do_block(new ForEachLoop(iter_decls, iters_guard, pb.block()).setContract(pb.contract()), true);

        blocks.pop();
        result = res;
    }

    /**
     * pass to do_block
     */
    @Override
    public void visit(ForEachLoop s) {
        Contract c = s.getContract();
        if (c == null) Fail("for each loop without iteration contract");
        result = do_block(s, false);
    }

    /**
     * contract barrier(label) { S }
     * ==> barrier_main(local_vars);
     * declaring:
     *
     * contract barrier_main(local_vars);
     *
     * forall threads in label:par . contract
     * barrier_check(local_vars - thread_vars + thread_count_vars) { S }
     */
    @Override
    public void visit(ParallelBarrier pb) {
        if (blocks.empty()) {
            Fail("barrier outside of parallel block");
        }
        BlockStatement res = rewrite(pb.body());
        ContractBuilder main_cb = new ContractBuilder();
        ContractBuilder check_cb = new ContractBuilder();
        Hashtable<String, Type> main_vars = free_vars(pb);
        Hashtable<String, Type> check_vars = new Hashtable<String, Type>(main_vars);
        ParallelBlock blk = null;
        for (ParallelBlock b : blocks) {
            if (b.label().equals(pb.label())) {
                blk = b;
            }
        }
        if (blk == null) {
            Fail("Block %s not found on block stack", pb.label());
        }
        ArrayList<ASTNode> guard_list = new ArrayList<ASTNode>();
        ArrayList<DeclarationStatement> guard_decls = new ArrayList<DeclarationStatement>();

        for (DeclarationStatement decl : blk.itersJava()) {
            ASTNode tmp = create.expression(StandardOperator.Member, create.unresolved_name(decl.name()), decl.initJava());
            guard_list.add(tmp);
            tmp = create.expression(StandardOperator.Size, decl.initJava());
            tmp = create.expression(StandardOperator.GT, tmp, create.constant(0));
            check_cb.requires(tmp);
            check_cb.ensures(tmp);
            guard_decls.add(create.field_decl(decl.name(), decl.getType()));
            check_vars.remove(decl.name());
            check_vars.putAll(free_vars(decl.initJava()));
        }

        ASTNode iters_guard = create.fold(StandardOperator.And, guard_list);
        DeclarationStatement iters_decl[] = guard_decls.toArray(new DeclarationStatement[0]);
        for (ASTNode clause : ASTUtils.conjuncts(pb.contract().pre_condition, StandardOperator.Star)) {
            if (clause.getType().isBoolean()) {
                check_cb.requires(create.forall(iters_guard, clause, iters_decl));
            } else {
                check_cb.requires(create.starall(iters_guard, clause, iters_decl));
            }
        }
        for (ASTNode clause : ASTUtils.conjuncts(pb.contract().post_condition, StandardOperator.Star)) {
            if (clause.getType().isBoolean()) {
                check_cb.ensures(create.forall(iters_guard, clause, iters_decl));
            } else {
                check_cb.ensures(create.starall(iters_guard, clause, iters_decl));
            }
        }
        check_cb.appendInvariant(pb.contract().invariant);
        count++;
        String main_name = "barrier_main_" + count;
        String check_name = "barrier_check_" + count;
        rewrite(pb.contract(), main_cb);
        for (ASTNode ib : inv_blocks) {
            if (ib instanceof ParallelInvariant) {
                ParallelInvariant inv = (ParallelInvariant) ib;
                if (pb.invs().contains(inv.label())) {
                    check_cb.requires(inv.inv());
                    check_cb.ensures(inv.inv());
                }
            } else {
                Abort("unexpected kind of invariant: %s", ib.getClass());
            }
        }
        currentTargetClass.add(create.method_decl(
                create.primitive_type(PrimitiveSort.Void),
                check_cb.getContract(),
                check_name,
                gen_pars(check_vars),
                res
        ));
        currentTargetClass.add(create.method_decl(
                create.primitive_type(PrimitiveSort.Void),
                main_cb.getContract(),
                main_name,
                gen_pars(main_vars),
                null
        ));
        result = gen_call(main_name, main_vars);
    }

    /**
     * atomic(a, b, c) {S}
     * ==> inhale ?; S; exhale ?
     *
     * if a is one of the invariant labels on the stack, do inhale invariant / exhale invariant
     * otherwise, find a predicate ending with csl_invariant in the class denoted by the type of a.
     * then do inhale pred; unfold pred // fold pred; exhale pred
     * arguments from the predicate presumably must be in local scope, perhaps just for this?
     */
    @Override
    public void visit(ParallelAtomic pa) {
        ASTNode atomicStat = rewrite(pa.block());
        BlockStatement block;

        if (atomicStat instanceof BlockStatement) {
            block = (BlockStatement) atomicStat;
        } else {
            block = create.block(atomicStat);
        }

        for (ASTNode node : pa.synclistJava()) {
            if (node instanceof NameExpression) {
                NameExpression name = (NameExpression) node;
                if (name.getKind() == NameExpression.Kind.Label) {
                    boolean found = false;
                    for (ASTNode ib : inv_blocks) {
                        if (ib instanceof ParallelInvariant) {
                            ParallelInvariant inv = (ParallelInvariant) ib;
                            if (inv.label().equals(name.toString())) {
                                block.prepend(create.special(ASTSpecial.Kind.Inhale, inv.inv()));
                                block.append(create.special(ASTSpecial.Kind.Exhale, inv.inv()).set_branch(LEAVE_ATOMIC));
                                found = true;
                            }
                        }
                    }
                    if (found) {
                        continue;
                    }
                    Fail("Could not find an invariant labeled %s", name);
                }
            }
            ClassType ct = (ClassType) node.getType();
            ASTClass cl = source().find(ct);
            String name = "csl_invariant";
            ArrayList<ASTNode> args = new ArrayList<ASTNode>();
            for (Method m : cl.dynamicMethods()) {
                if (m.name().endsWith("csl_invariant")) {
                    name = m.name();
                    for (DeclarationStatement d : m.getArgs()) {
                        args.add(create.local_name(d.name()));
                    }
                }
            }
            block.prepend(create.special(ASTSpecial.Kind.Unfold, create.invokation(rewrite(node), null, name, args)));
            block.prepend(create.special(ASTSpecial.Kind.Inhale, create.invokation(rewrite(node), null, name, args)));
            block.append(create.special(ASTSpecial.Kind.Fold, create.invokation(rewrite(node), null, name, args)));
            block.append(create.special(ASTSpecial.Kind.Exhale, create.invokation(rewrite(node), null, name, args)));
        }
        result = block;
    }

    /**
     * if is copied
     * put the guard on guard_stack when appropriate
     */
    @Override
    public void visit(IfStatement s) {
        IfStatement res = new IfStatement();
        res.setOrigin(s.getOrigin());
        int N = s.getCount();
        for (int i = 0; i < N; i++) {
            ASTNode guard = s.getGuard(i);
            if (guard != IfStatement.elseGuard()) guard = guard.apply(this);
            Debug("pushing guard");
            guard_stack.push(guard);
            ASTNode body = s.getStatement(i);
            body = body.apply(this);
            Debug("popping guard");
            guard_stack.pop();
            res.addClause(guard, body);
        }
        result = res;
        return;
    }

    /**
     * send Perm ==> send_body(locals)
     * recv Perm ==> recv_body(locals)
     *
     * declaring:
     *
     * loop_invariant; require Perm send_body(locals);
     * loop_invariant; ensure Perm recv_body(locals);
     *
     * and put the statement on send_recv_map
     */
    public void visit(ASTSpecial statement) {
        if(!statement.isSpecial(Kind.Send) && !statement.isSpecial(Kind.Recv)) {
            super.visit(statement);
            return;
        }

        int N = counter.incrementAndGet();
        ContractBuilder cb = new ContractBuilder();
        BranchOrigin branch;

        Hashtable<String, Type> statementVars = new Hashtable<String, Type>();
        NameScanner scanner = new NameScanner(statementVars);
        statement.accept(scanner);
        loop_invariant.accept(scanner);

        switch (statement.kind) {
            case Send:
                if (current_label == null) {
                    Fail("send in unlabeled position");
                }
                if (send_recv_map.get(current_label) != null) {
                    Fail("more than one send/recv in block %s.", current_label);
                }
                send_recv_map.put(current_label, new SendRecvInfo(statement));
                // create method contract
                //and call the method

                String send_name = "send_body_" + N;

                ArrayList<DeclarationStatement> send_decl = new ArrayList<DeclarationStatement>();// declaration of parameters for send_check
                ArrayList<ASTNode> send_args = new ArrayList<ASTNode>();// the arguments to the host_check method

                for (String var : statementVars.keySet()) {
                    send_decl.add(create.field_decl(var, copy_rw.rewrite(statementVars.get(var))));
                    send_args.add(create.unresolved_name(var));
                }

                cb = new ContractBuilder();
                cb.requires(copy_rw.rewrite(loop_invariant));
                cb.ensures(copy_rw.rewrite(loop_invariant));

                cb.requires(copy_rw.rewrite(statement.getArg(0))); //update new contract

                Method send_body = create.method_decl(
                        create.primitive_type(PrimitiveSort.Void),
                        cb.getContract(), //method contract
                        send_name,
                        send_decl.toArray(new DeclarationStatement[0]),
                        null // no body
                );

                //Error management  --> line numbers, origins , ...
                branch = new BranchOrigin("Send Statement", null);
                OriginWrapper.wrap(null, send_body, branch);
                //Error management  --> line numbers, origins , ...

                //Check for side conditions
                if (!hasNonNegDistArg(statement)) {
                    super.visit(statement);
                    Fail("The distance of dependence in the \"send\" statement should be positive.");
                }
                ///Check for side conditions

                currentTargetClass.add_dynamic(send_body);

                result = create.invokation(null, null, send_name, send_args.toArray(new ASTNode[0]));
                break;
            case Recv:
                // create method contract
                // and call the method
                if (current_label == null) {
                    Fail("recv in unlabeled position");
                }
                if (send_recv_map.get(current_label) != null) {
                    Fail("more than one send/recv in block %s.", current_label);
                }
                send_recv_map.put(current_label, new SendRecvInfo(statement));

                String recv_name = "recv_body_" + N;
                ArrayList<DeclarationStatement> recv_decl = new ArrayList<DeclarationStatement>();// declaration of parameters for send_check
                ArrayList<ASTNode> recv_args = new ArrayList<ASTNode>();// the arguments to the host_check method

                for (String var : statementVars.keySet()) {
                    recv_decl.add(create.field_decl(var, copy_rw.rewrite(statementVars.get(var))));
                    recv_args.add(create.unresolved_name(var));
                }

                cb = new ContractBuilder();
                cb.requires(copy_rw.rewrite(loop_invariant));
                cb.ensures(copy_rw.rewrite(loop_invariant));

                cb.ensures(copy_rw.rewrite(statement.getArg(0))); //update new contract

                Method recv_body = create.method_decl(
                        create.primitive_type(PrimitiveSort.Void),
                        cb.getContract(), //method contract
                        recv_name,
                        recv_decl.toArray(new DeclarationStatement[0]),
                        null // no body
                );

                //Error management  --> line numbers, origins , ...
                branch = new BranchOrigin("Recv Statement", null);
                OriginWrapper.wrap(null, recv_body, branch);
                //Error management  --> line numbers, origins , ...

                //Check for side conditions

                if (!hasNonNegDistArg(statement)) {
                    super.visit(statement);
                    Fail("The distance of dependence in the \"recv\" statement should be positive.");
                }
                ///Check for side conditions
                currentTargetClass.add_dynamic(recv_body);
                result = create.invokation(null, null, recv_name, recv_args.toArray(new ASTNode[0]));

                break;

        }

    }

    /**
     * Something something iteration or block dependencies check
     */
    private void gen_consistent(ParallelRegion region, ParallelBlock pb1, ParallelBlock pb2, boolean guard) {
        ASTNode pre_condition = region.contract().pre_condition;
        HashMap<NameExpression, ASTNode> map1 = new HashMap<NameExpression, ASTNode>();
        Substitution sigma1 = new Substitution(source(), map1);
        HashMap<NameExpression, ASTNode> map2 = new HashMap<NameExpression, ASTNode>();
        Substitution sigma2 = new Substitution(source(), map2);
        ContractBuilder cb = new ContractBuilder();
        if (region.contract() != null) {
            cb.requires(region.contract().invariant);
        }
        cb.requires(pre_condition);
        Hashtable<String, Type> main_vars = free_vars(pre_condition);
        ArrayList<ASTNode> list = new ArrayList<ASTNode>();
        int N = 0;

        for (DeclarationStatement decl : pb1.itersJava()) {
            String name = "x" + (++N);
            main_vars.put(name, decl.getType());
            map1.put(create.unresolved_name(decl.name()), create.unresolved_name(name));
            OperatorExpression range = (OperatorExpression) decl.initJava();
            cb.requires(create.expression(
                    StandardOperator.LTE, rewrite(range.arg(0)), create.unresolved_name(name))
            );
            cb.requires(create.expression(
                    StandardOperator.LT, create.unresolved_name(name), rewrite(range.arg(1)))
            );
        }

        for (DeclarationStatement decl : pb2.itersJava()) {
            String name = "x" + (++N);
            main_vars.put(name, decl.getType());
            map2.put(create.unresolved_name(decl.name()), create.unresolved_name(name));
            OperatorExpression range = (OperatorExpression) decl.initJava();
            cb.requires(create.expression(
                    StandardOperator.LTE, rewrite(range.arg(0)), create.unresolved_name(name))
            );
            cb.requires(create.expression(
                    StandardOperator.LT, create.unresolved_name(name), rewrite(range.arg(1)))
            );
        }

        ASTNode args[] = new ASTNode[N];
        for (int i = 0; i < N; ) {
            args[i] = create.argument_name("x" + (++i));
        }
        for (ASTNode clause : ASTUtils.conjuncts(pb1.contract().pre_condition, StandardOperator.Star)) {
            if (clause.getType().isResource()) {
                list.add(sigma1.rewrite(clause));
            }
        }
        for (ASTNode clause : ASTUtils.conjuncts(pb2.contract().pre_condition, StandardOperator.Star)) {
            if (clause.getType().isResource()) {
                list.add(sigma2.rewrite(clause));
            }
        }
        ASTNode body = create.special(ASTSpecial.Kind.Assert, create.fold(StandardOperator.Star, list));
        if (guard) {
            body = create.ifthenelse(create.expression(StandardOperator.Not,
                    create.invokation(null, null, "before_" + pb1.label() + "_" + pb2.label(), args)),
                    create.block(
                            body
                    )
            );
        }
        currentTargetClass.add(create.method_decl(
                create.primitive_type(PrimitiveSort.Void),
                cb.getContract(),
                "check_" + pb1.label() + "_" + pb2.label(),
                gen_pars(main_vars),
                create.block(
                        body
                )
        ));

    }

    private boolean hasNonNegDistArg(ASTSpecial e) {
        ///1. check the distance of dependence constant expressions
        if (e.getArg(2) instanceof ConstantExpression) {
            return getConstantInt((ConstantExpression) e.getArg(2)) > 0;
        } else {
            return false;
        }
    }


    private ASTNode do_block(ForEachLoop s, final boolean contract) {
        Contract c = s.getContract();
        loop_invariant = c.invariant;
        ASTNode res = null;
        Hashtable<String, Type> body_vars = free_vars(s.body, c, s.guard);
        //Hashtable<String,Type> iters=new Hashtable<String,Type>();
        Hashtable<String, Type> main_vars = new Hashtable<String, Type>(body_vars);
        for (DeclarationStatement decl : s.decls) {
            //iters.put(decl.name,decl.getType());
            main_vars.remove(decl.name());
        }

        int N = counter.incrementAndGet();
        String main_name = "loop_main_" + N;
        String body_name = "loop_body_" + N;
        ContractBuilder main_cb = new ContractBuilder();
        ContractBuilder body_cb = new ContractBuilder();

        for (ASTNode clause : ASTUtils.conjuncts(c.invariant, StandardOperator.Star)) {
            Hashtable<String, Type> clause_vars = free_vars(clause);
            for (DeclarationStatement decl : s.decls) {
                if (clause_vars.get(decl.name()) != null) {
                    Fail("illegal iteration invariant at %s", clause.getOrigin());
                }
            }
            if (clause.getType().isBoolean() || clause.isa(StandardOperator.Value)) {
                main_cb.requires(rewrite(clause));
                main_cb.ensures(rewrite(clause));
                body_cb.requires(rewrite(clause));
                body_cb.ensures(rewrite(clause));
            } else {
                Fail("illegal iteration invariant at %s", clause.getOrigin());
            }
        }

        DeclarationStatement iter_decls[] = s.decls;
        for (ASTNode clause : ASTUtils.conjuncts(c.pre_condition, StandardOperator.Star)) {
            if (clause.getType().isBoolean()) {
                main_cb.requires(create.forall(copy_rw.rewrite(s.guard), rewrite(clause), iter_decls));
            } else if (clause.isa(StandardOperator.ReducibleSum)) {
                OperatorExpression expr = (OperatorExpression) clause;
                main_cb.requires(create.expression(StandardOperator.Perm,
                        copy_rw.rewrite(expr.arg(0)),
                        create.reserved_name(ASTReserved.FullPerm)
                ));
            } else if (is_a_quantified(clause, Binder.Star, StandardOperator.ReducibleSum)) {
                BindingExpression bclause = (BindingExpression) clause;
                OperatorExpression expr = (OperatorExpression) bclause.main;
                main_cb.requires(create.starall(
                        bclause.select,
                        create.expression(StandardOperator.Perm,
                                copy_rw.rewrite(expr.arg(0)),
                                create.reserved_name(ASTReserved.FullPerm)
                        ),
                        bclause.getDeclarations()
                ));
            } else {
                main_cb.requires(create.starall(copy_rw.rewrite(s.guard), rewrite(clause), iter_decls));
            }
        }
        for (ASTNode clause : ASTUtils.conjuncts(c.post_condition, StandardOperator.Star)) {
            if (clause.getType().isBoolean()) {
                main_cb.ensures(create.forall(copy_rw.rewrite(s.guard), rewrite(clause), iter_decls));
            } else if (clause.isa(StandardOperator.Contribution)) {
                OperatorExpression expr = (OperatorExpression) clause;
                main_cb.ensures(create.expression(StandardOperator.Perm,
                        copy_rw.rewrite(expr.arg(0)),
                        create.reserved_name(ASTReserved.FullPerm)
                ));
                main_cb.ensures(create.expression(StandardOperator.EQ,
                        copy_rw.rewrite(expr.arg(0)),
                        plus(create.expression(StandardOperator.Old, copy_rw.rewrite(expr.arg(0))),
                                create.summation(copy_rw.rewrite(s.guard), rewrite(expr.arg(1)), iter_decls))
                ));
            } else if (is_a_quantified(clause, Binder.Star, StandardOperator.Contribution)) {
                BindingExpression bclause = (BindingExpression) clause;
                OperatorExpression expr = (OperatorExpression) bclause.main;
                main_cb.ensures(create.starall(
                        bclause.select,
                        create.expression(StandardOperator.Perm,
                                copy_rw.rewrite(expr.arg(0)),
                                create.reserved_name(ASTReserved.FullPerm)
                        ),
                        bclause.getDeclarations()
                ));
                main_cb.ensures(create.forall(
                        bclause.select,
                        create.expression(StandardOperator.EQ,
                                copy_rw.rewrite(expr.arg(0)),
                                plus(create.expression(StandardOperator.Old, copy_rw.rewrite(expr.arg(0))),
                                        create.summation(copy_rw.rewrite(s.guard), rewrite(expr.arg(1)), iter_decls))
                        ),
                        bclause.getDeclarations()
                ));
            } else {
                main_cb.ensures(create.starall(copy_rw.rewrite(s.guard), rewrite(clause), iter_decls));
            }
        }

        if (contract) {
            res = main_cb.getContract();
        } else {
            DeclarationStatement main_pars[] = gen_pars(main_vars);
            currentTargetClass.add(create.method_decl(
                    create.primitive_type(PrimitiveSort.Void),
                    main_cb.getContract(),
                    main_name,
                    main_pars,
                    null
            ));
        }
        body_cb.requires(rewrite(s.guard));
        body_cb.ensures(rewrite(s.guard));

        for (ASTNode clause : ASTUtils.conjuncts(c.pre_condition, StandardOperator.Star)) {
            if (clause.isa(StandardOperator.ReducibleSum)) {
                OperatorExpression expr = (OperatorExpression) clause;
                body_cb.requires(create.expression(StandardOperator.PointsTo,
                        copy_rw.rewrite(expr.arg(0)),
                        create.reserved_name(ASTReserved.FullPerm),
                        create.expression(StandardOperator.Cast, expr.arg(0).getType(), create.constant(0))
                ));
            } else if (is_a_quantified(clause, Binder.Star, StandardOperator.ReducibleSum)) {
                BindingExpression bclause = (BindingExpression) clause;
                OperatorExpression expr = (OperatorExpression) bclause.main;
                body_cb.requires(create.starall(
                        bclause.select,
                        create.expression(StandardOperator.Perm,
                                copy_rw.rewrite(expr.arg(0)),
                                create.reserved_name(ASTReserved.FullPerm)
                        ),
                        bclause.getDeclarations()
                ));
                body_cb.requires(create.forall(
                        bclause.select,
                        create.expression(StandardOperator.EQ,
                                copy_rw.rewrite(expr.arg(0)),
                                create.constant(0)
                        ),
                        bclause.getDeclarations()
                ));
            } else {
                body_cb.requires(rewrite(clause));
            }
        }
        for (ASTNode clause : ASTUtils.conjuncts(c.post_condition, StandardOperator.Star)) {
            if (clause.isa(StandardOperator.Contribution)) {
                OperatorExpression expr = (OperatorExpression) clause;
                body_cb.ensures(create.expression(StandardOperator.PointsTo,
                        rewrite(expr.arg(0)),
                        create.reserved_name(ASTReserved.FullPerm),
                        rewrite(expr.arg(1))
                ));
            } else if (is_a_quantified(clause, Binder.Star, StandardOperator.Contribution)) {
                BindingExpression bclause = (BindingExpression) clause;
                OperatorExpression expr = (OperatorExpression) bclause.main;
                body_cb.ensures(create.starall(
                        bclause.select,
                        create.expression(StandardOperator.Perm,
                                rewrite(expr.arg(0)),
                                create.reserved_name(ASTReserved.FullPerm)
                        ),
                        bclause.getDeclarations()
                ));
                body_cb.ensures(create.forall(
                        bclause.select,
                        create.expression(StandardOperator.EQ,
                                rewrite(expr.arg(0)),
                                rewrite(expr.arg(1))
                        ),
                        bclause.getDeclarations()
                ));
            } else {
                body_cb.ensures(rewrite(clause));
            }
        }

        DeclarationStatement body_pars[] = gen_pars(body_vars);
        currentTargetClass.add(create.method_decl(
                create.primitive_type(PrimitiveSort.Void),
                body_cb.getContract(),
                body_name,
                body_pars,
                rewrite(s.body)
        ));
        if (s.decls.length > 0) {
            String var_name = s.decls[s.decls.length - 1].name();
            check_send_recv(body_pars, var_name, s.guard);
        }
        if (!contract) {
            res = gen_call(main_name, main_vars);
        }
        loop_invariant = null;
        return res;
    }

    private boolean is_a_quantified(ASTNode expr, Binder bd, StandardOperator op) {
        if (expr instanceof BindingExpression) {
            BindingExpression b = (BindingExpression) expr;
            if (b.binder == bd) {
                return b.main.isa(op);
            }
        }
        return false;
    }

    protected void check_send_recv(DeclarationStatement[] body_decl, String var_name, ASTNode guard) {
        ContractBuilder cb;
        BranchOrigin branch;
        for (String R : send_recv_map.keySet()) {
            SendRecvInfo recv_entry = send_recv_map.get(R);
            if (recv_entry.stat.isSpecial(Kind.Recv)) {
                ASTSpecial recv = (ASTSpecial) recv_entry.stat;
                String S = ((NameExpression) recv.getArg(1)).getName();
                SendRecvInfo send_entry = send_recv_map.get(S);
                if (send_entry == null || !send_entry.stat.isSpecial(Kind.Send)) {
                    Fail("unmatched recv");
                }
                ASTSpecial send = (ASTSpecial) send_entry.stat;
                if (!R.equals(((NameExpression) send.getArg(1)).getName())) {
                    Fail("wrong label in send");
                }
                int dr = getConstantInt(recv.getArg(2));
                int ds = getConstantInt(send.getArg(2));
                if (dr != ds) {
                    Fail("distances of send(%d) and recv(%d) are different", ds, dr);
                }
                // create shift substitution.
                HashMap<NameExpression, ASTNode> shift_map = new HashMap<NameExpression, ASTNode>();
                NameExpression name = create.argument_name(var_name);
                shift_map.put(name, create.expression(StandardOperator.Minus, name, create.constant(dr)));
                Substitution shift = new Substitution(null, shift_map);
                // create guard check.
                cb = new ContractBuilder();
                cb.requires(loop_invariant);
                cb.ensures(loop_invariant);
                cb.requires(guard);
                for (ASTNode g : recv_entry.guards) {
                    cb.requires(g);
                }
                cb.ensures(create.expression(StandardOperator.LTE,
                        create.constant(dr), create.argument_name(var_name)
                ));
                for (ASTNode g : send_entry.guards) {
                    cb.ensures(shift.rewrite(g));
                }
                Method guard_method = create.method_decl(
                        create.primitive_type(PrimitiveSort.Void),
                        cb.getContract(),
                        String.format("guard_check_%s_%s", S, R),
                        body_decl,
                        create.block()
                );
                branch = new BranchOrigin("Guard Check", null);
                OriginWrapper.wrap(null, guard_method, branch);
                currentTargetClass.add_dynamic(guard_method);
                //create resource check
                cb = new ContractBuilder();
                cb.requires(loop_invariant);
                cb.ensures(loop_invariant);

                cb.requires(guard);
                // lower bound is already guaranteed by guard check.
                //cb.requires(create.expression(StandardOperator.LTE,
                //    create.constant(dr),create.argument_name(var_name)
                //));
                for (ASTNode g : send_entry.guards) {
                    cb.requires(shift.rewrite(g));
                }
                for (ASTNode g : recv_entry.guards) {
                    cb.requires(g);
                }
                cb.requires(shift.rewrite(send.getArg(0)));
                for (ASTNode g : send_entry.guards) {
                    cb.ensures(shift.rewrite(g));
                }
                cb.ensures(copy_rw.rewrite(recv.getArg(0)));
                Method resource_method = create.method_decl(
                        create.primitive_type(PrimitiveSort.Void),
                        cb.getContract(),
                        String.format("resource_check_%s_%s", S, R),
                        body_decl,
                        create.block()
                );
                branch = new BranchOrigin("Resource Check", null);
                OriginWrapper.wrap(null, resource_method, branch);
                currentTargetClass.add_dynamic(resource_method);

            }
            // unmatched send statements are wasteful, but not incorrect.
        }
        send_recv_map.clear();
    }

    private int getConstantInt(ASTNode arg) {
        IntegerValue v = (IntegerValue) ((ConstantExpression) arg).value();
        return v.value();
    }
}
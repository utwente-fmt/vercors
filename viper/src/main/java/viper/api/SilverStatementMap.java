package viper.api;

import hre.ast.AssertOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import hre.util.Triple;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.langspecific.c.*;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import static hre.lang.System.Abort;

public class SilverStatementMap<T, E, S> implements ASTMapping<S> {

    public final StatementFactory<Origin, T, E, S> create;
    public final ExpressionFactory<Origin, T, E> ef;
    public final TypeFactory<T> tf;
    public HashSet<Origin> refuted = null;
    public HashSet<Origin> satCheckAsserts = new HashSet<>();
    private SilverTypeMap<T> type;
    private SilverExpressionMap<T, E> expr;
    private boolean valid_null = false;

    public SilverStatementMap(ViperAPI<Origin, T, E, S, ?, ?, ?> backend, SilverTypeMap<T> type, SilverExpressionMap<T, E> expr) {
        this.create = backend.stat;
        ef = backend.expr;
        tf = backend._type;
        this.type = type;
        this.expr = expr;
    }

    @Override
    public void pre_map(ASTNode n) {
    }

    @Override
    public S post_map(ASTNode n, S res) {
        if (res == null) {
            if (valid_null) {
                valid_null = false;
            } else {
                throw new HREError("cannot map %s to statement", n.getClass());
            }
        }
        if (valid_null) {
            throw new HREError("valid null set while anser is non-null", n.getClass());
        }
        return res;
    }

    @Override
    public S map(StandardProcedure p) {

        return null;
    }

    @Override
    public S map(ConstantExpression e) {

        return null;
    }

    @Override
    public S map(OperatorExpression e) {
        throw new HREError("cannot map operator %s to statement", e.operator());
    }

    @Override
    public S map(NameExpression e) {
        E eq = e.apply(expr);
        eq = ef.eq(e.getOrigin(), eq, eq);
        return create.assert_(e.getOrigin(), eq);
    }

    @Override
    public S map(ClassType t) {

        return null;
    }

    @Override
    public S map(FunctionType t) {

        return null;
    }

    @Override
    public S map(PrimitiveType t) {

        return null;
    }

    @Override
    public S map(RecordType t) {

        return null;
    }

    @Override
    public S map(MethodInvokation e) {
        Origin o = e.getOrigin();
        Method m = e.getDefinition();
        String name = m.name();
        ArrayList<E> args = new ArrayList<E>();
        ArrayList<E> outs = new ArrayList<E>();
        ArrayList<Triple<Origin, String, T>> pars = new ArrayList<Triple<Origin, String, T>>();
        ArrayList<Triple<Origin, String, T>> rets = new ArrayList<Triple<Origin, String, T>>();
        int N = e.getArity();
        DeclarationStatement decl[] = m.getArgs();
        for (int i = 0; i < N; i++) {
            if (decl[i].isValidFlag(ASTFlags.OUT_ARG) && decl[i].getFlag(ASTFlags.OUT_ARG)) {
                outs.add(e.getArg(i).apply(expr));
                rets.add(new Triple<Origin, String, T>(decl[i].getOrigin(), decl[i].name(), decl[i].getType().apply(type)));
            } else {
                args.add(e.getArg(i).apply(expr));
                pars.add(new Triple<Origin, String, T>(decl[i].getOrigin(), decl[i].name(), decl[i].getType().apply(type)));
            }
        }
        return create.method_call(o, name, args, outs, pars, rets);
    }

    @Override
    public S map(BlockStatement s) {
        ArrayList<S> stats = new ArrayList<S>();
        for (ASTNode n : s) {
            S res = n.apply(this);
            if (res != null) stats.add(res);
        }
        return create.block(s.getOrigin(), stats);
    }

    @Override
    public S map(IfStatement s) {
        int i = s.getCount() - 1;
        S res;
        if (s.getGuard(i).isConstant(true)) {
            res = s.getStatement(i).apply(this);
            i = i - 1;
        } else {
            res = create.block(s.getOrigin(), new ArrayList<S>());
        }
        while (i >= 0) {
            res = create.if_then_else(s.getOrigin(), s.getGuard(i).apply(expr), s.getStatement(i).apply(this), res);
            i = i - 1;
        }
        return res;
    }

    @Override
    public S map(ReturnStatement s) {

        return null;
    }

    @Override
    public S map(AssignmentStatement s) {
        Origin origin = s.getOrigin();
        ASTNode location = s.location();
        ASTNode expression = s.expression();
        return assignment(origin, location, expression);
    }

    public S assignment(Origin origin, ASTNode location, ASTNode expression) {
        if (expression.isa(StandardOperator.NewSilver)) {
            ArrayList<String> names = new ArrayList<String>();
            ArrayList<T> types = new ArrayList<T>();

            OperatorExpression op = (OperatorExpression) expression;
            for (ASTNode arg : op.argsJava()) {
                Dereference d = (Dereference) arg;
                names.add(d.field());
                types.add(d.getType().apply(type));
            }

            return create.new_object(origin, location.apply(expr), names, types);
        } else {
            return create.assignment(origin, location.apply(expr), expression.apply(expr));
        }
    }

    @Override
    public S map(DeclarationStatement s) {
        return null;
    }

    @Override
    public S map(LoopStatement s) {
        Origin o = s.getOrigin();
        if (s.getInitBlock() != null) Abort("not a while loop");
        if (s.getExitGuard() != null) Abort("not a while loop");
        ArrayList<Triple<Origin, String, T>> locals = new ArrayList<Triple<Origin, String, T>>();
        ArrayList<S> stats = new ArrayList<S>();
        VerCorsProgramFactory.split_block(type, this, (BlockStatement) s.getBody(), locals, stats);
        ArrayList<E> invs = new ArrayList<E>();
        for (ASTNode inv : ASTUtils.conjuncts(s.getContract().invariant, StandardOperator.Star)) {
            invs.add(inv.apply(expr));
        }
        return create.while_loop(o, s.getEntryGuard().apply(expr), invs, locals, create.block(o, stats));
    }

    @Override
    public S map(Method m) {

        return null;
    }

    @Override
    public S map(ASTClass c) {

        return null;
    }

    @Override
    public S map(BindingExpression e) {

        return null;
    }

    @Override
    public S map(Dereference e) {

        return null;
    }

    @Override
    public S map(Lemma lemma) {

        return null;
    }

    @Override
    public S map(ParallelBarrier parallelBarrier) {

        return null;
    }

    @Override
    public S map(ParallelBlock parallelBlock) {

        return null;
    }

    @Override
    public S map(Contract contract) {

        return null;
    }

    @Override
    public S map(ASTSpecial special) {
        switch (special.kind) {
            case Goto:
                return create.goto_(special.getOrigin(), special.args[0].toString());
            case Label:
                return create.label(special.getOrigin(), special.args[0].toString(), new ArrayList<E>());
            case Inhale:
                return create.inhale(special.getOrigin(), special.args[0].apply(expr));
            case Exhale:
                return create.exhale(special.getOrigin(), special.args[0].apply(expr));
            case Assert:
                if (special.getOrigin() instanceof AssertOrigin) {
                    satCheckAsserts.add(special.getOrigin());
                }
                return create.assert_(special.getOrigin(), special.args[0].apply(expr));
            case Refute: {
                refuted.add(special.getOrigin());
                return create.refute(special.getOrigin(), special.args[0].apply(expr));
            }
            case Assume:
                return create.inhale(special.getOrigin(), special.args[0].apply(expr));
            case Fold:
                return create.fold(special.getOrigin(), special.args[0].apply(expr));
            case Unfold:
                return create.unfold(special.getOrigin(), special.args[0].apply(expr));
            case Fresh:
                throw new HREError("Fresh is no longer supported in viper. See https://github.com/utwente-fmt/vercors/issues/383");
            default:
                throw new HREError("cannot map special %s", special.kind);
        }
    }

    @Override
    public S map(VariableDeclaration variableDeclaration) {

        return null;
    }

    @Override
    public S map(TupleType tupleType) {

        return null;
    }

    @Override
    public S map(AxiomaticDataType adt) {

        return null;
    }

    @Override
    public S map(Axiom axiom) {

        return null;
    }

    @Override
    public S map(Hole hole) {

        return null;
    }

    @Override
    public S map(ActionBlock actionBlock) {
        return null;
    }

    @Override
    public S map(TypeExpression t) {

        return null;
    }

    @Override
    public S map(ForEachLoop s) {

        return null;
    }

    @Override
    public S map(ParallelAtomic parallelAtomic) {

        return null;
    }

    @Override
    public S map(ParallelInvariant inv) {
        return null;
    }

    @Override
    public S map(NameSpace nameSpace) {

        return null;
    }

    @Override
    public S map(TryCatchBlock tcb) {
        return null;
    }

    @Override
    public S map(FieldAccess a) {
        if (a.value() == null) {
            throw new HREError("Field read expression in statement map.");
        } else {
            ASTNode var = new Dereference(a.object(), a.name());
            var.setOrigin(a);
            return assignment(a.getOrigin(), var, a.value());
        }
    }

    @Override
    public S map(ParallelRegion region) {
        return null;
    }

    @Override
    public S map(TypeVariable v) {
        return null;
    }

    @Override
    public S map(StructValue v) {
        return null;
    }

    @Override
    public S map(VectorBlock vb) {
        return null;
    }

    private <F extends ASTNode> ArrayList<E> do_names(List<F> args) {
        ArrayList<E> names = new ArrayList<E>();
        for (ASTNode n : args) {
            names.add(n.apply(expr));
        }
        return names;
    }

    private <F extends ASTNode> ArrayList<E> do_names(F args[]) {
        return do_names(Arrays.asList(args));
    }

    @Override
    public S map(Constraining c) {
        throw new HREError("Constraining is no longer supported in viper. See https://github.com/utwente-fmt/vercors/issues/383");
    }

    @Override
    public S map(Switch s) {
        return null;
    }

    @Override
    public S map(TryWithResources t) {
        return null;
    }

    @Override
    public S map(Synchronized sync) {
        return null;
    }

    @Override
    public S map(CFunctionType t) {
        return null;
    }

    @Override
    public S map(OMPParallel parallel) {
        return null;
    }

    @Override
    public S map(OMPSection section) {
        return null;
    }

    @Override
    public S map(OMPSections sections) {
        return null;
    }

    @Override
    public S map(OMPFor loop) {
        return null;
    }

    @Override
    public S map(OMPParallelFor loop) {
        return null;
    }

    @Override
    public S map(OMPForSimd loop) {
        return null;
    }

    @Override
    public S map(InlineQuantifierPattern pattern) {
        return null;
    }

    @Override
    public S map(CatchClause cc) {
        return null;
    }

    @Override
    public S map(SignalsClause cc) {
        return null;
    }

    @Override
    public S map(KernelInvocation ki) {
        return null;
    }
}

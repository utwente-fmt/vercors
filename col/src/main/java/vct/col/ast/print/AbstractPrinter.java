// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.print;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.ast.TrackingOutput;
import hre.lang.HREError;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.Hole;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.SignalsClause;
import vct.col.ast.syntax.Syntax;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.TypeExpression;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.AbstractVisitor;

import java.util.ArrayList;
import java.util.List;

/**
 * This class contains the generic code for pretty printing expressions
 * based on a given syntax.
 * <p>
 * This class will use the precedences supplied by the syntax to minimize the
 * number of parenthesis.
 *
 * @author sccblom
 */
public class AbstractPrinter extends AbstractVisitor<Object> {

    private static final Origin missing = new MessageOrigin("unknown location");
    protected Syntax syntax;
    protected int current_precedence;
    protected TrackingOutput out;
    protected boolean in_expr;
    protected int expr_level;

    public AbstractPrinter(Syntax syntax, TrackingOutput out) {
        this.syntax = syntax;
        this.out = out;
        current_precedence = 0;
    }

    // use expression mode until exit from current visit
    public void setExpr() {
        if (!in_expr) {
            in_expr = true;
            expr_level = 1;
        }
    }

    // use inline mode in next accept call
    public void nextExpr() {
        if (!in_expr) {
            in_expr = true;
            expr_level = 0;
        }
    }

    public void pre_visit(ASTNode node) {
        super.pre_visit(node);
        if (in_expr) {
            expr_level++;
        }
        Origin o = node.getOrigin();
        if (o == null) {
            o = missing;
        }
        out.enter(node.getOrigin());
    }

    public void post_visit(ASTNode node) {
        out.leave(node.getOrigin());
        if (in_expr) {
            expr_level--;
            in_expr = (expr_level > 0);
        }
        super.post_visit(node);
    }

    @Override
    public void visit(TypeExpression t) {
        switch (t.operator()) {
            case Extern:
                out.printf("extern ");
                t.firstType().apply(this);
                break;
            case Static:
                out.printf("static ");
                t.firstType().apply(this);
                break;
            case Const:
                out.printf("const ");
                t.firstType().apply(this);
                break;
            case Kernel:
                out.printf("__kernel ");
                t.firstType().apply(this);
                break;
            case Global:
                out.printf("__global ");
                t.firstType().apply(this);
                break;
            case Local:
                out.printf("__local ");
                t.firstType().apply(this);
                break;
            case Unsigned:
                out.printf("unsigned ");
                t.firstType().apply(this);
                break;
            case PointerTo:
                t.firstType().apply(this);
                out.printf("*");
                break;
            default:
                throw new HREError("Missing case: %s", t.operator());
        }
    }

    public void visit(Hole hole) {
        out.printf("[.]");
    }

    public void visit(PrimitiveType t) {
        String s = syntax.getPrimitiveType(t.sort);
        if (s == null) throw new Error("unsupported primitive type: " + t.sort);
        out.printf(s);
    }

    public void visit(NameExpression e) {
        ASTReserved word = e.reserved();
        if (word == null) {
            out.print(e.getName());
        } else {
            String s = syntax.getSyntax(word);
            if (s == null) {
                throw Failure("reserved word %s not part of langauge", word);
            }
            out.print(s);
        }
        if (!in_expr) {
            out.lnprintf(";");
        }
    }

    public void visit(MethodInvokation e) {
        setExpr();
        if (e.object() != null) {
            // TODO: manage precedence properly.
            e.object().accept(this);
            out.printf(".");
        }
        out.printf("%s", e.method());
        if (e.dispatch() != null) {
            out.printf("@");
            e.dispatch().accept(this);
        }
        out.printf("(");
        int N = e.getArity();
        if (N > 0) {
            int precedence = current_precedence;
            current_precedence = 0;
            e.getArg(0).accept(this);
            for (int i = 1; i < N; i++) {
                out.print(",");
                current_precedence = 0;
                e.getArg(i).accept(this);
            }
            current_precedence = precedence;
        }
        out.print(")");
    }

    public void visit(KernelInvocation e) {
        setExpr();
        out.printf("%s", e.method());
        out.printf("<<<");
        e.blockCount().accept(this);
        out.printf(", ");
        e.threadCount().accept(this);
        out.printf(">>>(");
        boolean first = true;
        for (ASTNode arg : e.javaArgs()) {
            if (!first) out.printf(", ");
            arg.accept(this);
            first = false;
        }
        out.printf(")");
    }

    public void visit(OperatorExpression e) {
        StandardOperator op = e.operator();
        String op_syntax[] = syntax.getSyntax(op);
        if (op_syntax == null) {
            throw new Error("no syntax defined for " + op + " operation");
        }
        int N = op.arity();
        ASTNode args[] = e.argsJava().toArray(new ASTNode[0]);
        setExpr();
        if (N < 0) {
            out.print(op_syntax[0]);
            if (args.length > 0) {
                args[0].accept(this);
            }
            for (int i = 1; i < args.length; i++) {
                out.print(op_syntax[1]);
                args[i].accept(this);
            }
            out.print(op_syntax[2]);
        } else {
            if (op_syntax[0] != null && op_syntax[0].length() > 0) out.printf("%s ", op_syntax[0]);
            for (int i = 0; i < N; i++) {
                if (i > 0) out.printf(" %s ", op_syntax[i]);
                args[i].accept(this);
            }
            if (op_syntax[0] != null && op_syntax[N].length() > 0) out.printf(" %s", op_syntax[N]);
        }
    }

    public void visit(ConstantExpression ce) {
        if (!in_expr) Abort("constant %s outside of expression for %s", ce, ce.getOrigin());
        out.print(ce.toString());
    }

    public void visit(ASTSpecial s) {
        switch (s.kind) {
            default:
                if (s.args.length == 0) {
                    out.printf("%s;%n", s.kind);
                } else {
                    setExpr();
                    out.printf("%s ", s.kind);
                    if (s.args[0] == null) {
                        out.print("null");
                    } else {
                        s.args[0].accept(this);
                    }
                    for (int i = 1; i < s.args.length; i++) {
                        out.printf(", ");
                        if (s.args[i] == null) {
                            out.print("null");
                        } else {
                            s.args[i].accept(this);
                        }
                    }
                    out.println(";");
                }
                break;
        }
    }

    @Override
    public void visit(Contract contract) {
        if (contract != null) {
            for (DeclarationStatement d : contract.given) {
                out.printf("given ");
                d.accept(this);
                out.lnprintf("");
            }
            for (ASTNode e : ASTUtils.conjuncts(contract.invariant, StandardOperator.Star)) {
                out.printf("loop_invariant ");
                nextExpr();
                e.accept(this);
                out.lnprintf(";");
            }
            List<ASTNode> contextElems = new ArrayList<>();
            for (ASTNode pre : ASTUtils.conjuncts(contract.pre_condition, StandardOperator.Star)) {
                boolean added = false;
                for (ASTNode post : ASTUtils.conjuncts(contract.post_condition, StandardOperator.Star)) {
                    if (pre.equals(post)) {
                        contextElems.add(pre);
                        printContractElement(pre, "context");
                        added = true;
                    }
                }
                if (!added) {
                    printContractElement(pre, "requires");
                }
            }
            for (ASTNode post : ASTUtils.conjuncts(contract.post_condition, StandardOperator.Star)) {
                if (!contextElems.contains(post)) {
                    printContractElement(post, "ensures");
                }
            }
            for (DeclarationStatement d : contract.yields) {
                out.printf("yields ");
                d.accept(this);
                out.lnprintf("");
            }
            for (SignalsClause sc : contract.signals) {
                sc.accept(this);
            }
            if (contract.modifies != null) {
                out.printf("modifies ");
                if (contract.modifies.length == 0) {
                    out.lnprintf("\\nothing;");
                } else {
                    nextExpr();
                    contract.modifies[0].accept(this);
                    for (int i = 1; i < contract.modifies.length; i++) {
                        out.printf(", ");
                        nextExpr();
                        contract.modifies[i].accept(this);
                    }
                    out.lnprintf(";");
                }
            }
            if (contract.accesses != null) {
                out.printf("accessible ");
                if (contract.accesses.length == 0) {
                    out.lnprintf("\\nothing;");
                } else {
                    nextExpr();
                    contract.accesses[0].accept(this);
                    for (int i = 1; i < contract.accesses.length; i++) {
                        out.printf(", ");
                        nextExpr();
                        contract.accesses[i].accept(this);
                    }
                    out.lnprintf(";");
                }
            }
        }
    }

    protected void printContractElement(ASTNode expr, String contractHead) {
        out.printf(contractHead + " ");
        nextExpr();
        if (expr instanceof MethodInvokation)
            out.print("(");
        expr.accept(this);
        if (expr instanceof MethodInvokation)
            out.print(")");
        out.lnprintf(";");
    }
}


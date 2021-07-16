package vct.col.util;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.util.RecursiveVisitor;

import java.util.HashSet;
import java.util.Set;

public class ControlFlowAnalyzer extends RecursiveVisitor<Set<ASTNode>> {

    public ControlFlowAnalyzer(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(Method m) {
        m.getBody().setPredecessor(m);
        m.getBody().accept(this);
    }

    @Override
    public void visit(BlockStatement block) {
        Set<ASTNode> res;
        int N = block.getLength();
        res = new HashSet<ASTNode>();
        res.add(block);
        for (int i = 0; i < N; i++) {
            ASTNode S = block.getStatement(i);
            S.setPredecessor(res);
            res = S.apply(this);
            if (res == null) {
                Abort("missing case: %s", S.getClass());
            }
        }
        result = res;
    }

    @Override
    public void visit(AssignmentStatement s) {
        result = new HashSet<ASTNode>();
        result.add(s);
    }

    @Override
    public void visit(MethodInvokation s) {
        result = new HashSet<ASTNode>();
        result.add(s);
    }

    @Override
    public void visit(OperatorExpression e) {
        result = new HashSet<ASTNode>();
        result.add(e);
    }

    @Override
    public void visit(ASTSpecial e) {
        result = new HashSet<ASTNode>();
        result.add(e);
    }

    @Override
    public void visit(ParallelBarrier pb) {
        result = new HashSet<ASTNode>();
        result.add(pb);
    }

    @Override
    public void visit(ParallelBlock pb) {
        Set<ASTNode> res = new HashSet<ASTNode>();
        res.add(pb);
        pb.block().setPredecessor(pb);
        pb.block().accept(this);
        result = res;
    }

    @Override
    public void visit(IfStatement s) {
        Set<ASTNode> res = new HashSet<ASTNode>();
        int N = s.getCount();
        for (int i = 0; i < N; i++) {
            s.getStatement(i).setPredecessor(s);
            res.addAll(s.getStatement(i).apply(this));
        }
        result = res;
    }

    @Override
    public void visit(DeclarationStatement d) {
        result = new HashSet<ASTNode>();
        result.add(d);
    }

    @Override
    public void visit(LoopStatement s) {
        Set<ASTNode> res = s.getBody().apply(this);
        res.add(s);
        s.getBody().setPredecessor(res);
        result = res;
    }

}

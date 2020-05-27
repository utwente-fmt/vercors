package vct.experiments.learn;

import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.FieldAccess;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.stmt.composite.ActionBlock;
import vct.col.ast.stmt.composite.ForEachLoop;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.composite.Lemma;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.ParallelAtomic;
import vct.col.ast.stmt.composite.ParallelBarrier;
import vct.col.ast.stmt.composite.ParallelBlock;
import vct.col.ast.stmt.composite.ParallelInvariant;
import vct.col.ast.stmt.composite.ParallelRegion;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.composite.VectorBlock;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.VariableDeclaration;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.TypeVariable;
import vct.col.ast.util.RecursiveVisitor;

public class CountVisitor extends RecursiveVisitor<NodeCounter> {
  
  private NodeCounter counter;
  
  public CountVisitor(ProgramUnit pu) {
    super(pu);
    this.counter = new NodeCounter();
  }
  
  public void count() {
    for (ASTDeclaration entry : source().get()) {
      entry.accept(this);
    }
  }
  
  public NodeCounter getCounter() {
    return counter;
  }
  
  @Override
  public void visit(ActionBlock ab) {
    counter.count(ab.getClass().getName());
    super.visit(ab);
  }
  
  @Override
  public void visit(AssignmentStatement as) {
    counter.count(as.getClass().getName());
    super.visit(as);
  }
  
  @Override
  public void visit(ConstantExpression ce) {
    counter.count(ce.getClass().getName());
    super.visit(ce);
  }
  
  @Override
  public void visit(Dereference d) {
    counter.count(d.getClass().getName());
    super.visit(d);
  }
  
  @Override
  public void visit(BindingExpression be) {
    counter.count(be.getClass().getName(), be.binder.toString());
    super.visit(be);
  }
  
  @Override
  public void visit(MethodInvokation mi) {
    counter.count(mi.getClass().getName());
    super.visit(mi);
  }
  
  @Override
  public void visit(OperatorExpression e) {
    counter.count(e.getClass().getName(), e.operator().toString());
    super.visit(e);
  }
  
  public void visit(StructValue sv) {
    counter.count(sv.getClass().getName());
    super.visit(sv);
  }
  
  @Override
  public void visit(FieldAccess fa) {
    counter.count(fa.getClass().getName());
    super.visit(fa);
  }
  
  @Override
  public void visit(ForEachLoop fol) {
    counter.count(fol.getClass().getName());
    super.visit(fol);
  }
  
  @Override
  public void visit(IfStatement is) {
    counter.count(is.getClass().getName());
    super.visit(is);
  }
  
  @Override
  public void visit(Lemma l) {
    counter.count(l.getClass().getName());
    super.visit(l);
  }
  
  @Override
  public void visit(LoopStatement ls) {
    counter.count(ls.getClass().getName());
    super.visit(ls);
  }
  
  @Override
  public void visit(NameExpression ne) {
    counter.count(ne.getClass().getName());
    super.visit(ne);
  }
  
  @Override
  public void visit(ParallelAtomic pa) {
    counter.count(pa.getClass().getName());
    super.visit(pa);
  }
  
  @Override
  public void visit(ParallelBarrier pb) {
    counter.count(pb.getClass().getName());
    super.visit(pb);
  }
  
  @Override
  public void visit(ParallelBlock pb) {
    counter.count(pb.getClass().getName());
    super.visit(pb);
  }
  
  @Override
  public void visit(ParallelInvariant pi) {
    counter.count(pi.getClass().getName());
    super.visit(pi);
  }
  
  @Override
  public void visit(ParallelRegion pr) {
    counter.count(pr.getClass().getName());
    super.visit(pr);
  }
  
  @Override
  public void visit(ReturnStatement rs) {
    counter.count(rs.getClass().getName());
    super.visit(rs);
  }
  
  @Override
  public void visit(Switch s) {
    counter.count(s.getClass().getName());
    super.visit(s);
  }
  
  @Override
  public void visit(TryCatchBlock tcb) {
    counter.count(tcb.getClass().getName());
    super.visit(tcb);
  }
  
  @Override
  public void visit(TypeVariable tv) {
    counter.count(tv.getClass().getName());
    super.visit(tv);
  }
  
  @Override
  public void visit(VariableDeclaration vd) {
    counter.count(vd.getClass().getName());
    super.visit(vd);
  }
  
  @Override
  public void visit(VectorBlock vb) {
    counter.count(vb.getClass().getName());
    super.visit(vb);
  }
  
}

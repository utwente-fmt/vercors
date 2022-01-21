package vct.col.util;

import java.util.EnumSet;

import vct.col.ast.expr.Binder;
import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.ForEachLoop;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.ParallelBlock;
import vct.col.ast.stmt.composite.ParallelInvariant;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.composite.Synchronized;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.util.RecursiveVisitor;

public class FeatureScanner extends RecursiveVisitor<Object> {
  public FeatureScanner(){
    super(null,null);
  }

  private boolean has_iteration_contracts=false;
  private boolean has_finally = false;
  private boolean has_return = false;
  private boolean has_catch = false;
  private boolean has_throwing_method_calls = false;
  private boolean has_throwing_methods = false;
  private EnumSet<StandardOperator> ops_used=EnumSet.noneOf(StandardOperator.class);
  private EnumSet<ASTSpecial.Kind> specials_used=EnumSet.noneOf(ASTSpecial.Kind.class);
  private EnumSet<Binder> binders_used=EnumSet.noneOf(Binder.class);

  public static FeatureScanner scan(ASTNode node) {
    FeatureScanner fs = new FeatureScanner();
    node.accept(fs);
    return fs;
  }

  public boolean usesOperator(StandardOperator op){
    return ops_used.contains(op);
  }
  
  public boolean usesSpecial(ASTSpecial.Kind op){
    return specials_used.contains(op);
  }

  public boolean usesFinally() {
    return has_finally;
  }

  public boolean usesReturn() {
    return has_return;
  }

  public boolean usesCatch() {
    return has_catch;
  }

  public boolean usesThrowingMethodCalls() {
    return has_throwing_method_calls;
  }

  public void pre_visit(ASTNode node){
    super.pre_visit(node);
  }

  @Override
  public void visit(ASTSpecial s){
    specials_used.add(s.kind);
    super.visit(s);
  }
  
  @Override
  public void visit(Method m){
    has_throwing_methods |= m.signals.length > 0;
    super.visit(m);
  }
  @Override
  public void visit(BindingExpression e){
    binders_used.add(e.binder());
    super.visit(e);
  }
  
  @Override
  public void visit(ASTClass c) {
    int N=c.getStaticCount();
    for(int i=0;i<N;i++){
      ASTNode node=c.getStatic(i);
      node.accept(this);
    }    
    N=c.getDynamicCount();
    for(int i=0;i<N;i++){
      c.getDynamic(i).accept(this);
    }
  }
  
  public void visit(ParallelBlock pb){
    super.visit(pb);
  }

  public void visit(ParallelInvariant inv){
    super.visit(inv);
  }

  public void visit(ForEachLoop s){
    super.visit(s);
    has_iteration_contracts=true;
  }
  
  public static boolean isIterationContract(Contract c){
    if (c==null) return false;
    return (c.pre_condition != Contract.default_true || c.post_condition != Contract.default_true);
  }
  
  public void visit(LoopStatement s){
    super.visit(s);
    if (has_iteration_contracts) return;
    has_iteration_contracts=isIterationContract(s.getContract());
  }
  
  public void visit(OperatorExpression e){
    super.visit(e);
    ops_used.add(e.operator());
  }

  public void visit(TryCatchBlock tryCatchBlock) {
    super.visit(tryCatchBlock);

    has_finally |= tryCatchBlock.after() != null;
    has_catch |= tryCatchBlock.numCatches() > 0;
  }

  public void visit(ReturnStatement returnStatement) {
    super.visit(returnStatement);

    has_return = true;
  }

  public void visit(Switch switchStatement) {
    super.visit(switchStatement);

  }

  public void visit(Synchronized synchronizedStatement) {
    super.visit(synchronizedStatement);

  }

  public void visit(MethodInvokation mi) {
    super.visit(mi);

    if (mi.getDefinition() != null && mi.getDefinition().getContract() != null) {
      has_throwing_method_calls |= mi.getDefinition().canThrowSpec();
    }
  }

  public boolean hasThrowingMethods() {
    return has_throwing_methods;
  }
}

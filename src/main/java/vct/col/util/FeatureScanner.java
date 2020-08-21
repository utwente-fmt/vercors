package vct.col.util;

import java.util.EnumSet;
import java.util.HashSet;

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
import vct.col.ast.stmt.composite.VectorBlock;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.ast.type.Type;

public class FeatureScanner extends RecursiveVisitor<Object> {

  public FeatureScanner(){
    super(null,null);
  }
  
  private boolean has_parallel_blocks=false;
  private boolean has_statics=false;
  private boolean has_dynamics=false;
  private boolean has_doubles=false;
  private boolean has_longs=false;
  private boolean has_processes=false;
  private boolean has_inheritance=false;
  private boolean has_kernels=false;
  private boolean has_iteration_contracts=false;
  private boolean has_finally = false;
  private boolean has_return = false;
  private boolean has_switch = false;
  private boolean has_synchronized_modifier;
  private boolean has_synchronized_statement = false;
  private boolean has_catch = false;
  private boolean has_throwing_method_calls = false;
  private boolean uses_csl=false;
  private EnumSet<StandardOperator> ops_used=EnumSet.noneOf(StandardOperator.class);
  private EnumSet<ASTSpecial.Kind> specials_used=EnumSet.noneOf(ASTSpecial.Kind.class);
  private EnumSet<Binder> binders_used=EnumSet.noneOf(Binder.class);
  
  private HashSet<Class<? extends ASTNode>> nodes=new HashSet<Class<? extends ASTNode>>();

  public static FeatureScanner scan(ASTNode node) {
    FeatureScanner fs = new FeatureScanner();
    node.accept(fs);
    return fs;
  }
  
  public boolean hasVectorBlocks(){
    return nodes.contains(VectorBlock.class);
  }
  
  public boolean usesCSL(){
    return uses_csl;
  }
  public boolean usesOperator(StandardOperator op){
    return ops_used.contains(op);
  }
  
  public boolean usesSpecial(ASTSpecial.Kind op){
    return specials_used.contains(op);
  }
 
  public boolean usesParallelBlocks(){
    return has_parallel_blocks;
  }
  
  public boolean usesDoubles(){
    return has_doubles;
  }
  
  public boolean usesSummation(){
    return binders_used.contains(Binder.Sum);
  }
  
  public boolean usesLongs(){
    return has_longs;
  }
  
  public boolean usesProcesses(){
    return has_processes;
  }
  
  public boolean hasStaticItems(){
    return has_statics;
  }

  public boolean hasDynamicItems(){
    return has_dynamics;
  }
  
  public boolean usesInheritance(){
    return has_inheritance;
  }
  
  public boolean usesKernels(){
    return has_kernels;
  }
  
  public boolean usesIterationContracts(){
    return has_iteration_contracts;
  }

  public boolean usesFinally() {
    return has_finally;
  }

  public boolean usesReturn() {
    return has_return;
  }

  public boolean usesSwitch() {
    return has_switch;
  }

  public boolean usesSynchronizedStatement() {
    return has_synchronized_statement;
  }

  public boolean usesSynchronizedModifier() {
    return has_synchronized_modifier;
  }

  public boolean usesCatch() {
    return has_catch;
  }

  public boolean usesThrowingMethodCalls() {
    return has_throwing_method_calls;
  }

  public void pre_visit(ASTNode node){
    super.pre_visit(node);
    Type t=node.getType();
    if (t!=null){
      if (t.isDouble()) has_doubles=true;
      if (t.isPrimitive(PrimitiveSort.Long)) has_longs=true;
      if (t.isPrimitive(PrimitiveSort.Process)) has_processes=true;
    }
    nodes.add(node.getClass());
  }

  private HashSet<String> pragmas=new HashSet<String>();
  
  @Override
  public void visit(ASTSpecial s){
    if(s.kind==Kind.Pragma){
      String pragma=((ASTSpecial)s).args[0].toString().split(" ")[0];
      pragmas.add(pragma);
    }
    specials_used.add(s.kind);
    super.visit(s);
  }
  
  @Override
  public void visit(Method m){
    uses_csl |= m.name().equals("csl_invariant");
    has_synchronized_modifier |= m.isSynchronized();
    super.visit(m);
  }
  @Override
  public void visit(BindingExpression e){
    binders_used.add(e.binder());
    super.visit(e);
  }
  
  @Override
  public void visit(ASTClass c) {
    if (c.kind==ASTClass.ClassKind.Kernel){
      has_kernels=true;
      return;
    }
    if (c.super_classes.length > 0 || c.implemented_classes.length > 0) {
      Warning("detected inheritance");
      has_inheritance=true;
    }
    int N=c.getStaticCount();
    for(int i=0;i<N;i++){
      ASTNode node=c.getStatic(i);
      if (!(node instanceof ASTClass)) {
        has_statics=true;
      }
      node.accept(this);
    }    
    N=c.getDynamicCount();
    for(int i=0;i<N;i++){
      c.getDynamic(i).accept(this);
    }
  }
  
  public void visit(ParallelBlock pb){
    super.visit(pb);
    has_parallel_blocks=true;
  }

  public void visit(ParallelInvariant inv){
    super.visit(inv);
    has_parallel_blocks=true;
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
  public boolean usesPragma(String string) {
    return pragmas.contains(string);
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

    has_switch = true;
  }

  public void visit(Synchronized synchronizedStatement) {
    super.visit(synchronizedStatement);

    has_synchronized_statement = true;
  }

  public void visit(MethodInvokation mi) {
    super.visit(mi);

    if (mi.getDefinition() != null && mi.getDefinition().getContract() != null) {
      has_throwing_method_calls |= mi.getDefinition().canThrowSpec();
    }
  }
}

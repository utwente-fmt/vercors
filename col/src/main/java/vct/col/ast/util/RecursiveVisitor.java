package vct.col.ast.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import scala.collection.JavaConverters;
import scala.collection.Seq;
import vct.col.ast.langspecific.c.*;
import vct.col.ast.stmt.composite.Switch.Case;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;

public class RecursiveVisitor<T> extends ASTFrame<T> implements ASTVisitor<T> {

  protected boolean auto_before_after=true;
  
  public RecursiveVisitor(ASTFrame<T> share) {
    super(share);
  }

  public RecursiveVisitor(ProgramUnit source) {
    super(source,false);
  }
  public RecursiveVisitor(ProgramUnit source, ProgramUnit target) {
    super(source, target,false);
  }
  public RecursiveVisitor(ProgramUnit source,boolean do_scope) {
    super(source,do_scope);
  }
  public RecursiveVisitor(ProgramUnit source, ProgramUnit target,boolean do_scope) {
    super(source, target,do_scope);
  }

  @Override
  public void pre_visit(ASTNode n) {
    enter(n);
  }

  @Override
  public void post_visit(ASTNode n) {
    if(n instanceof BeforeAfterAnnotations && auto_before_after){
      BeforeAfterAnnotations baa=(BeforeAfterAnnotations)n;
      enter_before(n);
      dispatch(baa.get_before());
      leave_before(n);
      enter_after(n);
      dispatch(baa.get_after());
      leave_after(n);
    }
    auto_before_after=true;
    leave(n);
  }

  @Override
  public void visit(StandardProcedure p) {
    // no chidren
  }

  @Override
  public void visit(ConstantExpression e) {
    // no children
  }

  @Override
  public void visit(ForEachLoop s){
    dispatch(s.decls);
    dispatch(s.guard);
    dispatch(s.body);
    dispatch(s.getContract());
  }
  
  @Override
  public void visit(OperatorExpression e) {
    for (ASTNode arg : e.argsJava()) {
      arg.accept(this);
    }    
  }

  @Override
  public void visit(NameExpression e) {
    // no children
  }

  @Override
  public void visit(ClassType t) {
    // no children
  }

  @Override
  public void visit(FunctionType t) {
    for (Type type : t.paramsJava()) {
      type.accept(this);
    }
    t.result().accept(this);
  }
  
  @Override
  public void visit(TypeExpression t) {
	for (Type type : t.typesJava()) {
	  type.accept(this);
	}
  }
  
  @Override
  public void visit(TupleType t) {
	for (Type type : t.typesJava()) {
	  type.accept(this);
	}
  }

  @Override
  public void visit(PrimitiveType t) {
	for (ASTNode arg : t.argsJava()) {
      arg.accept(this);
    }          
  }

  @Override
  public void visit(RecordType t) {
    int n = t.fieldCount();
    for (int i = 0; i < n; i++) {
      t.fieldType(i).accept(this);
    }
  }

  @Override
  public void visit(MethodInvokation e) {
    // TODO: fix dispatch(e.get_before());
    dispatch(e.object());
    for(ASTNode arg:e.getArgs()){
      arg.accept(this);
    }
    // TODO: fix dispatch(e.get_after());
  }
  
  private void dispatch(Contract c){
    if (c!=null){
      c.accept(this);
    }
  }
  private void dispatch(ASTNode ... objects) {
    for(ASTNode object:objects){
      if(object!=null){
        object.accept(this);
      }
    }
  }
  
  private <R extends ASTNode> void dispatch(List<R> nodes) {
    for (R node : nodes) {
      if (node != null) {
        node.accept(this);
      }
    }
  }

  private <R extends ASTNode> void dispatch(Seq<R> nodes) {
    for (R node : JavaConverters.seqAsJavaList(nodes)) {
      if (node != null) {
        node.accept(this);
      }
    }
  }

  public <E extends ASTNode, F extends ASTNode> void dispatch(Map<E,F> map){
    HashMap<E, F> res=new HashMap<E,F>();
    for(Map.Entry<E, F> entry:map.entrySet()){
      if (entry.getKey() != null) dispatch(entry.getKey());
      if (entry.getValue() != null) dispatch(entry.getValue());
    }
  }


  @Override
  public void visit(BlockStatement s) {
    int N=s.getLength();
    for(int i=0;i<N;i++){
      s.getStatement(i).accept(this);
    }
  }

  @Override
  public void visit(IfStatement s) {
    int N=s.getCount();
    for(int i=0;i<N;i++){
      s.getGuard(i).accept(this);
      s.getStatement(i).accept(this);
    }
    
  }

  @Override
  public void visit(ReturnStatement s) {
    dispatch(s.getExpression());
    dispatch(s.get_after());
  }

  @Override
  public void visit(AssignmentStatement s) {
    s.location().accept(this);
    s.expression().accept(this);
  }

  @Override
  public void visit(DeclarationStatement s) {
    s.getType().accept(this);
    dispatch(s.initJava());
  }

  @Override
  public void visit(LoopStatement s) {
    dispatch(s.get_before());
    dispatch(s.getInitBlock());
    dispatch(s.getEntryGuard());
    dispatch(s.getUpdateBlock());
    dispatch(s.getContract());
    s.getBody().accept(this);
    dispatch(s.getExitGuard());
    dispatch(s.get_after());
  }

  @Override
  public void visit(Method m) {
//    dispatch(m.getContract());
//    if (c!=null){
//      dispatch(c.pre_condition);
//      dispatch(c.post_condition);
//    }
    Contract c=m.getContract();
    if (c!=null){
      dispatch(c.given);
      dispatch(c.pre_condition);
      dispatch(c.invariant);
      // Yielded variables are not known before method starts.
      dispatch(c.yields);
      dispatch(c.signals);
    }
    dispatch(m.getArgs());
    dispatch(m.getBody());
    if (c!=null) {
      // TODO: this is where \result should be declared.
      dispatch(c.post_condition);      
    }
  }

  @Override
  public void visit(ActionBlock ab){
    dispatch(ab.history());
    dispatch(ab.fraction());
    dispatch(ab.process());
    dispatch(ab.action());
    // TODO: enable visiting map elements.
    //dispatch(ab.map().values().to);
    dispatch(ab.block());
  }
  
  @Override
  public void visit(ASTClass c){
    int N;
    N=c.getStaticCount();
    for(int i=0;i<N;i++){
      c.getStatic(i).accept(this);
    }
    N=c.getDynamicCount();
    for(int i=0;i<N;i++){
      c.getDynamic(i).accept(this);
    }
  }

  @Override
  public void visit(BindingExpression e) {
    int N=e.getDeclCount();
    for(int i=0;i<N;i++){
      e.getDeclaration(i).accept(this);
    }
    dispatch(e.result_type());
    dispatch(e.select());
    if (e.triggers()!=null){
      for(ASTNode tmp[]:e.javaTriggers()){
        dispatch(tmp);
      }
    }
    e.main().accept(this);
    if (e instanceof SetComprehension) {
      dispatch(((SetComprehension) e).variables());
    }
  }

  @Override
  public void visit(Dereference e){
    e.obj().accept(this);
  }
  
  @Override
  public void visit(Lemma lemma) {
    lemma.block().accept(this);
  }
  
  public void visit(ParallelAtomic pa) {
	for (ASTNode item : pa.synclistJava()) {
	  dispatch(item);
	}
    dispatch(pa.block());
  }
  
  public void visit(ParallelInvariant inv){
    dispatch(inv.inv());
    dispatch(inv.block());
  }

  public void visit(ParallelBarrier pb){
    dispatch(pb.contract());
    dispatch(pb.body());
  }

  public void visit(ParallelBlock pb){
    dispatch(pb.itersJava());
    dispatch(pb.contract());
    dispatch(pb.block());
  }
  
  public void visit(ParallelRegion region){
    dispatch(region.contract());
    dispatch(region.blocksJava());
  }

  public void visit(Contract c){
    dispatch(c.given);
    dispatch(c.yields);
    if (c.modifies != null) {
      dispatch(c.modifies);
    }
    if (c.accesses != null) {
      dispatch(c.accesses);
    }
    dispatch(c.invariant);
    dispatch(c.pre_condition);
    dispatch(c.post_condition);
    dispatch(c.signals);
  }

  public void visit(ASTSpecial s){
    for(ASTNode n:s.args){
      dispatch(n);
    }
  }
  
  @Override
  public void visit(VariableDeclaration decl) {
    dispatch(decl.basetype);
    for(ASTDeclaration d:decl.get()){
      dispatch(d);
    }
  }

  @Override
  public void visit(AxiomaticDataType adt){
    for(Axiom ax:adt.axiomsJava()){
      dispatch(ax);
    }
  }
  
  @Override
  public void visit(Axiom axiom){
    dispatch(axiom.rule());
  }
  
  @Override
  public void visit(Hole hole){
    dispatch(hole.get());
  }
  
  @Override
  public void visit(NameSpace ns){
    for (ASTNode n:ns){
      dispatch(n);
    }
  }

  @Override
  public void visit(TryCatchBlock tcb) {
    dispatch(tcb.main());
    for (CatchClause cc : tcb.catches()) {
        dispatch(cc);
    }
    dispatch(tcb.after());
  }

  @Override
  public void visit(TryWithResources t) {
    dispatch(t.javaResources());
    dispatch(t.main());
    for(CatchClause clause : t.clauses()) {
      enter(clause.block());
      dispatch(clause.javaCatchTypes());
      for(ASTNode stat : clause.block()) {
        dispatch(stat);
      }
      leave(clause.block());
    }
    dispatch((BlockStatement)t.after().orNull(null));
  }

  @Override
  public void visit(Synchronized sync) {
    dispatch(sync.expr());
    enter(sync);
    dispatch(sync.statement());
    leave(sync);
  }

  @Override
  public void visit(CFunctionType t) {
    dispatch(t.returnType());

    for(ParamSpec param : JavaConverters.seqAsJavaList(t.params())) {
      if(param.t().isDefined()) {
        dispatch(param.t().get());
      }
    }
  }

  @Override
  public void visit(OMPParallel parallel) {
    dispatch(parallel.contract());
    dispatch(parallel.block());
  }

  @Override
  public void visit(OMPSection section) {
    dispatch(section.block());
  }

  @Override
  public void visit(OMPSections sections) {
    dispatch(sections.block());
  }

  @Override
  public void visit(OMPFor loop) {
    dispatch(loop.loop());
  }

  @Override
  public void visit(OMPParallelFor loop) {
    dispatch(loop.loop());
  }

  @Override
  public void visit(OMPForSimd loop) {
    dispatch(loop.loop());
  }

  @Override
  public void visit(FieldAccess a) {
    dispatch(a.object());
    dispatch(a.value());
  }

  @Override
  public void visit(TypeVariable v) {
  }
  
  @Override
  public void visit(StructValue v) {
    dispatch(v.type());
    dispatch(v.valuesArray());
  }

  @Override
  public void visit(VectorBlock v) {
    dispatch(v.iter());
    dispatch(v.block());
  }

  @Override
  public void visit(Constraining c) {
    dispatch(c.varsJava());
    dispatch(c.block());
  }

  @Override
  public void visit(Switch s) {
    dispatch(s.expr);
    for(Case c:s.cases){
      for(ASTNode n:c.cases) dispatch(n);
      for(ASTNode n:c.stats) dispatch(n);
    }
  }

  @Override
  public void visit(CatchClause cc) {
    dispatch(cc.javaCatchTypes());
    dispatch(cc.block());
  }

  @Override
  public void visit(SignalsClause sc) {
    dispatch(sc.type());
    dispatch(sc.condition());
  }
}

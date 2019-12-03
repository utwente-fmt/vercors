// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.util;

import java.util.*;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.util.ClassName;


/**
 * Abstract AST visitor implementation which can skip cases and
 * distinguish between bad input and missing cases.
 * 
 * @author sccblom
 *
 */
public class AbstractVisitor<T> extends ASTFrame<T> implements ASTVisitor<T> {
/*
  Note: need a good way of creating and abstract class that will
  allow defining both valid input cases and cases which are ignored
  on purpose. Maybe a not only the 'case' equivalent but also
  a 'recursive' default would be useful.
*/
    
  protected Stack<ClassName> current_class_stack=new Stack<ClassName>();
  protected ClassName current_class=null;
  
  public AbstractVisitor(){
    this(null,null,false);
  }
  
  public AbstractVisitor(ProgramUnit source, boolean do_scope) {
    super(source,do_scope);
  }
  
  public AbstractVisitor(ProgramUnit source,ProgramUnit target,boolean do_scope) {
    super(source,target,do_scope);
  }
  
  public AbstractVisitor(ASTFrame<T> shared) {
    super(shared);
  }
 
  private final void visit_any(ASTNode any){
    java.lang.Class<? extends Object> any_class=any.getClass();
    Debug("for origin %s",any.getOrigin());
    throw new Error("missing case or invalid AST: "+any_class.getSimpleName()+" in "+this.getClass().getSimpleName());
  }
  
  @Override
  public void pre_visit(ASTNode n){
    this.enter(n);
    if (n instanceof ASTClass){
      // set current class and push.
      current_class=new ClassName(((ASTClass)n).getFullName());
      current_class_stack.push(current_class);
    }
  }
  
  @Override
  public void post_visit(ASTNode n){
    if (n instanceof ASTClass){
      // pop current class and reset.
      current_class_stack.pop();
      if (current_class_stack.isEmpty()){
        current_class=null;
      } else {
        current_class=current_class_stack.peek();
      }
    }
    this.leave(n);
  }
  
  @Override public void visit(StandardProcedure p){ visit_any(p); }

  @Override public void visit(ConstantExpression e){ visit_any(e); }
  
  @Override public void visit(OperatorExpression e){ visit_any(e); }
  
  @Override public void visit(NameExpression e){ visit_any(e); }

  @Override public void visit(ClassType t){ visit_any(t); }
  
  @Override public void visit(FunctionType t){ visit_any(t); }
  
  @Override public void visit(PrimitiveType t){ visit_any(t); }
  
  @Override public void visit(RecordType t){ visit_any(t); }

  @Override public void visit(MethodInvokation e){ visit_any(e); }

  @Override public void visit(BlockStatement s){ visit_any(s); }
  
  @Override public void visit(IfStatement s){ visit_any(s); }
  
  @Override public void visit(ReturnStatement s){ visit_any(s); }
  
  @Override public void visit(AssignmentStatement s){ visit_any(s); }

  @Override public void visit(DeclarationStatement s){ visit_any(s); }

  @Override public void visit(LoopStatement s){ visit_any(s); }

  @Override public void visit(Method m){ visit_any(m); }

  @Override public void visit(ASTClass c){ visit_any(c); }
  
  @Override public void visit(BindingExpression e){ visit_any(e); }
  
  @Override public void visit(Dereference e){ visit_any(e); }

  @Override public void visit(Lemma l){ visit_any(l); }
  
  @Override public void visit(ParallelAtomic pa){ visit_any(pa); }

  @Override public void visit(ParallelInvariant inv){ visit_any(inv); }

  @Override public void visit(ParallelBarrier pb){ visit_any(pb); }

  @Override public void visit(ParallelBlock pb){ visit_any(pb); }
  
  @Override public void visit(ParallelRegion pb){ visit_any(pb); }
  
  @Override public void visit(Contract c){ visit_any(c); }

  @Override public void visit(ASTSpecial s){ visit_any(s); }
  
  @Override public void visit(VariableDeclaration s) { visit_any(s); }
  
  @Override public void visit(TupleType t) { visit_any(t); }

  @Override public void visit(TypeExpression t) { visit_any(t); }

  @Override public void visit(AxiomaticDataType adt) { visit_any(adt); }

  @Override public void visit(Axiom axiom) { visit_any(axiom); }
  
  @Override public void visit(Hole hole) { visit_any(hole); }
  
  @Override public void visit(ActionBlock ab) { visit_any(ab); }

  @Override public void visit(ForEachLoop s) { visit_any(s); }

  @Override public void visit(NameSpace ns) { visit_any(ns); }

  @Override public void visit(TryCatchBlock tcb) { visit_any(tcb); }

  @Override public void visit(FieldAccess a) { visit_any(a); }
  
  @Override public void visit(TypeVariable v) { visit_any(v); }

  @Override public void visit(StructValue v) { visit_any(v); }

  @Override public void visit(VectorBlock v) { visit_any(v); }

  @Override public void visit(Constraining v) { visit_any(v); }

  @Override public void visit(Switch s) { visit_any(s); }

}



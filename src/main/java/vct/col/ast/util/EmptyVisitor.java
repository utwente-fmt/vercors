package vct.col.ast.util;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;

/**
 * Base implementation of an ASTVisitor which does nothing unless 
 * a visiting method is overridden.
 * 
 * @author Stefan Blom
 *
 * @param <T>
 */
public class EmptyVisitor<T> implements ASTVisitor<T> {

  protected T result;

  @Override
  public T getResult() {
    return result;
  }

  @Override
  public void pre_visit(ASTNode n) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void post_visit(ASTNode n) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(StandardProcedure p) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ConstantExpression e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(OperatorExpression e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(NameExpression e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ClassType t) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(FunctionType t) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(PrimitiveType t) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(RecordType t) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(MethodInvokation e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(BlockStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(IfStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ReturnStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(AssignmentStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(DeclarationStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(LoopStatement s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ForEachLoop s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Method m) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ASTClass c) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(BindingExpression e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Dereference e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Lemma lemma) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ParallelBarrier parallelBarrier) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ParallelBlock parallelBlock) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Contract contract) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ASTSpecial special) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(VariableDeclaration variableDeclaration) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TupleType tupleType) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(AxiomaticDataType adt) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Axiom axiom) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Hole hole) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ActionBlock actionBlock) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TypeExpression t) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ParallelAtomic parallelAtomic) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(NameSpace nameSpace) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TryCatchBlock tcb) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(FieldAccess a) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ParallelInvariant inv) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(ParallelRegion region) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TypeVariable v) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(StructValue v) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(VectorBlock vb) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Constraining c) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(Switch s) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(SignalsClause s) {

  }
  
}

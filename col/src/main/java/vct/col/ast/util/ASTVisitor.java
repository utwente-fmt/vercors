// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.util;

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

public interface ASTVisitor<T> {
  
  T getResult();
  
  void pre_visit(ASTNode n);
  
  void post_visit(ASTNode n);
  
  void visit(StandardProcedure p);
  
  void visit(StructValue v);
  
  void visit(ConstantExpression e);
  
  void visit(OperatorExpression e);
  
  void visit(NameExpression e);
  
  void visit(ClassType t);
  
  void visit(FunctionType t);
  
  void visit(PrimitiveType t);
  
  void visit(RecordType t);
  
  void visit(MethodInvokation e);

  void visit(BlockStatement s);
  
  void visit(IfStatement s);
  
  void visit(ReturnStatement s);
  
  void visit(AssignmentStatement s);

  void visit(DeclarationStatement s);
  
  void visit(LoopStatement s);
  
  void visit(ForEachLoop s);
  
  void visit(Method m);

  void visit(ASTClass c);

  void visit(BindingExpression e);

  void visit(Dereference e);

  void visit(Lemma lemma);

  void visit(ParallelBarrier parallelBarrier);

  void visit(ParallelBlock parallelBlock);

  void visit(Contract contract);

  void visit(ASTSpecial special);

  void visit(VariableDeclaration variableDeclaration);

  void visit(TupleType tupleType);

  void visit(AxiomaticDataType adt);

  void visit(Axiom axiom);

  void visit(Hole hole);

  void visit(ActionBlock actionBlock);

  void visit(TypeExpression t);

  void visit(ParallelAtomic parallelAtomic);

  void visit(NameSpace nameSpace);

  void visit(TryCatchBlock tcb);

  void visit(FieldAccess a);

  void visit(ParallelInvariant inv);
  
  void visit(ParallelRegion region);

  void visit(TypeVariable v);

  void visit(VectorBlock vb);
  
  void visit(Constraining c);

  void visit(Switch s);

  void visit(TryWithResources t);

  void visit(Synchronized sync);

  void visit(CFunctionType t);

  void visit(OMPParallel parallel);
  void visit(OMPSection section);
  void visit(OMPSections sections);
  void visit(OMPFor loop);
  void visit(OMPParallelFor loop);
  void visit(OMPForSimd loop);

  void visit(InlineQuantifierPattern pattern);
  void visit(CatchClause cc);
  void visit(SignalsClause sc);
  void visit(KernelInvocation ki);
}



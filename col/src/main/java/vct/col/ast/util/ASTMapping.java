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

public interface ASTMapping<R> {
  
  void pre_map(ASTNode n);
  
  R post_map(ASTNode n,R res);
  
  R map(StandardProcedure p);
  
  R map(StructValue v);
  
  R map(ConstantExpression e);
  
  R map(OperatorExpression e);
  
  R map(NameExpression e);
  
  R map(ClassType t);
  
  R map(FunctionType t);
  
  R map(PrimitiveType t);
  
  R map(RecordType t);
  
  R map(MethodInvokation e);

  R map(BlockStatement s);
  
  R map(IfStatement s);
  
  R map(ReturnStatement s);
  
  R map(AssignmentStatement s);

  R map(DeclarationStatement s);
  
  R map(LoopStatement s);
  
  R map(ForEachLoop s);
  
  R map(Method m);

  R map(ASTClass c);

  R map(BindingExpression e);

  R map(Dereference e);

  R map(Lemma lemma);

  R map(ParallelBarrier parallelBarrier);

  R map(ParallelBlock parallelBlock);

  R map(Contract contract);

  R map(ASTSpecial special);

  R map(VariableDeclaration variableDeclaration);

  R map(TupleType tupleType);

  R map(AxiomaticDataType adt);

  R map(Axiom axiom);

  R map(Hole hole);

  R map(ActionBlock actionBlock);

  R map(TypeExpression t);

  R map(ParallelAtomic parallelAtomic);

  R map(NameSpace ns);

  R map(TryCatchBlock tcb);

  R map(FieldAccess a);

  R map(ParallelInvariant inv);
  
  R map(ParallelRegion region);

  R map(TypeVariable v);

  R map(VectorBlock vb);

  R map(Constraining c);

  R map(Switch s);

  R map(TryWithResources t);

  R map(Synchronized sync);

  R map(CFunctionType t);

  R map(OMPParallel parallel);
  R map(OMPSection section);
  R map(OMPSections sections);
  R map(OMPFor loop);
  R map(OMPParallelFor loop);
  R map(OMPForSimd loop);

  R map(InlineQuantifierPattern pattern);
  R map(CatchClause cc);
  R map(SignalsClause sc);

  R map(KernelInvocation ki);
}

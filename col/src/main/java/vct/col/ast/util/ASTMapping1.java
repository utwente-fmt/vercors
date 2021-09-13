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

public interface ASTMapping1<R,A1> {
  
  void pre_map(ASTNode n, A1 a);
  R post_map(ASTNode n,R res,A1 a);
  R map(StandardProcedure p, A1 a);
  R map(StructValue v, A1 a);
  R map(ConstantExpression e, A1 a);
  R map(OperatorExpression e, A1 a);
  R map(NameExpression e, A1 a);
  R map(ClassType t, A1 a);
  R map(FunctionType t, A1 a);
  R map(PrimitiveType t,A1 a);
  R map(RecordType t,A1 a);
  R map(MethodInvokation e, A1 a);

  R map(BlockStatement s, A1 a);
  
  R map(IfStatement s,A1 a);
  
  R map(ReturnStatement s, A1 a);
  
  R map(AssignmentStatement s, A1 a);

  R map(DeclarationStatement s,A1 a);
  
  R map(LoopStatement s, A1 a);
  
  R map(ForEachLoop s, A1 a);
  
  R map(Method m,A1 a);

  R map(ASTClass c, A1 a);

  R map(BindingExpression e, A1 a);

  R map(Dereference e, A1 a);

  R map(Lemma lemma,A1 a);

  R map(ParallelBarrier parallelBarrier,A1 a);

  R map(ParallelBlock parallelBlock,A1 a);

  R map(ParallelRegion region,A1 a);

  R map(Contract contract, A1 a);

  R map(ASTSpecial special, A1 a);

  R map(VariableDeclaration variableDeclaration,A1 a);

  R map(TupleType tupleType, A1 a);

  R map(AxiomaticDataType adt, A1 a);

  R map(Axiom axiom, A1 a);

  R map(Hole hole,A1 a);

  R map(ActionBlock actionBlock, A1 a);

  R map(TypeExpression t, A1 a);

  R map(ParallelAtomic pa, A1 a);
  
  R map(ParallelInvariant inv, A1 a);
  
  R map(NameSpace ns, A1 a);

  R map(TryCatchBlock tcb, A1 a);
  
  R map(FieldAccess s, A1 a);
  
  R map(TypeVariable v, A1 a);
  
  R map(VectorBlock vb,A1 a);
  
  R map(Constraining c, A1 a);

  R map(Switch s,A1 a);

  R map(TryWithResources t, A1 a);

  R map(Synchronized sync, A1 a);

  R map(CFunctionType t, A1 a);

  R map(OMPParallel parallel, A1 arg);
  R map(OMPSection section, A1 arg);
  R map(OMPSections sections, A1 arg);
  R map(OMPFor loop, A1 arg);
  R map(OMPParallelFor loop, A1 arg);
  R map(OMPForSimd loop, A1 arg);

  R map(InlineQuantifierPattern pattern, A1 arg);
  R map(CatchClause cc, A1 arg);
  R map(SignalsClause sc, A1 arg);

  R map(KernelInvocation ki, A1 arg);
}

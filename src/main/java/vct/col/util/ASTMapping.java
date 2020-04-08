package vct.col.util;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.langspecific.c.CFunctionType;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;

public interface ASTMapping<R> {
  
  public void pre_map(ASTNode n);
  
  public R post_map(ASTNode n,R res);
  
  public R map(StandardProcedure p);
  
  public R map(StructValue v);
  
  public R map(ConstantExpression e);
  
  public R map(OperatorExpression e);
  
  public R map(NameExpression e);
  
  public R map(ClassType t);
  
  public R map(FunctionType t);
  
  public R map(PrimitiveType t);
  
  public R map(RecordType t);
  
  public R map(MethodInvokation e);

  public R map(BlockStatement s);
  
  public R map(IfStatement s);
  
  public R map(ReturnStatement s);
  
  public R map(AssignmentStatement s);

  public R map(DeclarationStatement s);
  
  public R map(LoopStatement s);
  
  public R map(ForEachLoop s);
  
  public R map(Method m);

  public R map(ASTClass c);

  public R map(BindingExpression e);

  public R map(Dereference e);

  public R map(Lemma lemma);

  public R map(ParallelBarrier parallelBarrier);

  public R map(ParallelBlock parallelBlock);

  public R map(Contract contract);

  public R map(ASTSpecial special);

  public R map(VariableDeclaration variableDeclaration);

  public R map(TupleType tupleType);

  public R map(AxiomaticDataType adt);

  public R map(Axiom axiom);

  public R map(Hole hole);

  public R map(ActionBlock actionBlock);

  public R map(TypeExpression t);

  public R map(ParallelAtomic parallelAtomic);

  public R map(NameSpace ns);

  public R map(TryCatchBlock tcb);

  public R map(FieldAccess a);

  public R map(ParallelInvariant inv);
  
  public R map(ParallelRegion region);

  public R map(TypeVariable v);

  public R map(VectorBlock vb);

  public R map(Constraining c);

  public R map(Switch s);

  public R map(TryWithResources t);

  public R map(Synchronized sync);

  public R map(CFunctionType t);
}

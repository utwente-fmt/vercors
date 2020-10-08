package vct.col.ast.util;

import hre.lang.HREError;
import vct.col.ast.langspecific.c.*;
import vct.col.ast.langspecific.c.CFunctionType;
import vct.col.ast.stmt.decl.VariableDeclaration;
import vct.col.ast.stmt.composite.VectorBlock;
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
 * Undefined Mapping.
 * 
 * This mapping is meant to be used as the abstract base class for other mappings.
 * It returns null for every input, but also throws an error is the result is
 * null.
 * 
 * @author Stefan Blom
 *
 * @param <T>
 */
public class UndefinedMapping<T> implements ASTMapping<T> {

  @Override
  public void pre_map(ASTNode n) {
  }

  @Override
  public T post_map(ASTNode n, T res) {
    if(res==null){
      throw new HREError("mapping for %s is undefined",n);
    }
    return res;
  }

  @Override
  public T map(StandardProcedure p) {
    return null;
  }

  @Override
  public T map(StructValue v) {
    return null;
  }

  @Override
  public T map(ConstantExpression e) {
    return null;
  }

  @Override
  public T map(OperatorExpression e) {
    return null;
  }

  @Override
  public T map(NameExpression e) {
    return null;
  }

  @Override
  public T map(ClassType t) {
    return null;
  }

  @Override
  public T map(FunctionType t) {
    return null;
  }

  @Override
  public T map(PrimitiveType t) {
    return null;
  }

  @Override
  public T map(RecordType t) {
    return null;
  }

  @Override
  public T map(MethodInvokation e) {
    return null;
  }

  @Override
  public T map(BlockStatement s) {
    return null;
  }

  @Override
  public T map(IfStatement s) {
    return null;
  }

  @Override
  public T map(ReturnStatement s) {
    return null;
  }

  @Override
  public T map(AssignmentStatement s) {
    return null;
  }

  @Override
  public T map(DeclarationStatement s) {
    return null;
  }

  @Override
  public T map(LoopStatement s) {
    return null;
  }

  @Override
  public T map(ForEachLoop s) {
    return null;
  }

  @Override
  public T map(Method m) {
    return null;
  }

  @Override
  public T map(ASTClass c) {
    return null;
  }

  @Override
  public T map(BindingExpression e) {
    return null;
  }

  @Override
  public T map(Dereference e) {
    return null;
  }

  @Override
  public T map(Lemma lemma) {
    return null;
  }

  @Override
  public T map(ParallelBarrier parallelBarrier) {
    return null;
  }

  @Override
  public T map(ParallelBlock parallelBlock) {
    return null;
  }

  @Override
  public T map(Contract contract) {
    return null;
  }

  @Override
  public T map(ASTSpecial special) {
     return null;
  }

  @Override
  public T map(VariableDeclaration variableDeclaration) {
    return null;
  }

  @Override
  public T map(TupleType tupleType) {
    return null;
  }

  @Override
  public T map(AxiomaticDataType adt) {
    return null;
  }

  @Override
  public T map(Axiom axiom) {
    return null;
  }

  @Override
  public T map(Hole hole) {
    return null;
  }

  @Override
  public T map(ActionBlock actionBlock) {
    return null;
  }

  @Override
  public T map(TypeExpression t) {
    return null;
  }

  @Override
  public T map(ParallelAtomic parallelAtomic) {
    return null;
  }

  @Override
  public T map(NameSpace ns) {
    return null;
  }

  @Override
  public T map(TryCatchBlock tcb) {
    return null;
  }

  @Override
  public T map(FieldAccess a) {
     return null;
  }

  @Override
  public T map(ParallelInvariant inv) {
    return null;
  }

  @Override
  public T map(ParallelRegion region) {
    return null;
  }

  @Override
  public T map(TypeVariable v) {
    return null;
  }

  @Override
  public T map(VectorBlock vb) {
    return null;
  }

  @Override
  public T map(Constraining c) {
    return null;
  }

  @Override
  public T map(Switch s) {
    return null;
  }

  @Override
  public T map(TryWithResources t) {
    return null;
  }

  @Override
  public T map(Synchronized sync) {
    return null;
  }

  @Override
  public T map(CFunctionType t) {
    return null;
  }

  @Override
  public T map(OMPParallel parallel) {
    return null;
  }

  @Override
  public T map(OMPSection section) {
    return null;
  }

  @Override
  public T map(OMPSections sections) {
    return null;
  }

  @Override
  public T map(OMPFor loop) {
    return null;
  }

  @Override
  public T map(OMPParallelFor loop) {
    return null;
  }

  @Override
  public T map(OMPForSimd loop) {
    return null;
  }

  @Override
  public T map(InlineQuantifierPattern pattern) {
    return null;
  }

  @Override
  public T map(CatchClause cc) {
    return null;
  }

  @Override
  public T map(SignalsClause sc) {
    return null;
  }
}

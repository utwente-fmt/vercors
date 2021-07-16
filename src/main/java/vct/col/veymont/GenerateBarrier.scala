package vct.col.veymont

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util._

import scala.collection.mutable.ArrayBuffer

class GenerateBarrier(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(m: Method): Unit = {
    m.getParent match {
      case clazz: ASTClass if isThreadClassName(clazz.name) =>
        if (m.kind == Method.Kind.Constructor) {
          val newContract = new ContractBuilder()
          newContract.requires(create.expression(StandardOperator.NEQ, create.argument_name(getArgName(barrierFieldName)), create.reserved_name(ASTReserved.Null)))
          getBarrierAnnotations.foreach(newContract.ensures(_))
          rewrite(m.getContract, newContract)
          val b = getBlockOrThrow(m.getBody, "VeyMont Fail: expected BlockStatement in Method " + m.name)
          val newArgs = rewrite(m.getArgs) :+ create.field_decl(getArgName(barrierFieldName), getBarrierClass)
          val barAssign = create.assignment(create.field_name(barrierFieldName), create.argument_name(getArgName(barrierFieldName)))
          val newBody = create.block(rewrite(b.getStatements) :+ barAssign: _*)
          result = create.method_kind(m.kind, m.getReturnType, newContract.getContract, m.name, newArgs, newBody)
        } else if (m.kind == Method.Kind.Pure || m.kind == Method.Kind.Predicate) {
          result = copy_rw.rewrite(m)
        } else {
          val newContract = new ContractBuilder()
          getBarrierAnnotations.foreach(newContract.context(_))
          rewrite(m.getContract, newContract)
          result = create.method_kind(m.kind, m.getReturnType, newContract.getContract, m.name, m.getArgs, rewrite(m.getBody))
        }
      case _ =>
        result = copy_rw.rewrite(m)
    }
  }

  private def getBarrierAnnotations: List[OperatorExpression] = {
    List(create.expression(StandardOperator.Perm, create.field_name(barrierFieldName), create.reserved_name(ASTReserved.ReadPerm))
      , create.expression(StandardOperator.NEQ, create.field_name(barrierFieldName), create.reserved_name(ASTReserved.Null)))
  }

  override def visit(s: IfStatement): Unit = {
    val stats: Seq[BlockStatement] = (0 until s.getCount).map(s.getStatement)
      .map(getBlockOrThrow(_, "VeyMont Fail: expected BlockStatement in IfStatementCase"))
      .map(b => create.block(prependBarrier(b): _*))
    result = create.ifthenelse(rewrite(s.getGuard(0)), stats: _*)
  }

  override def visit(l: LoopStatement): Unit = {
    val b = getBlockOrThrow(l.getBody, "VeyMont Fail: expected BlockStatement in LoopStatement")
    val newContract = new ContractBuilder()
    getBarrierAnnotations.foreach(newContract.appendInvariant(_))
    rewrite(l.getContract, newContract)
    result = create.while_loop(rewrite(l.getEntryGuard), create.block(prependBarrier(b): _*), newContract.getContract)
  }

  private def prependBarrier(b: BlockStatement): Array[ASTNode] = getBarrierInvocation +: rewrite(b.getStatements)

  private def getBarrierInvocation: MethodInvokation = create.invokation(create.field_name(barrierFieldName), null, barrierAwait)

  override def visit(b: BlockStatement): Unit = {
    val nrLoops = b.getStatements.count(_.isInstanceOf[LoopStatement])
    if (nrLoops > 0) {
      val newStats = ArrayBuffer.empty[ASTNode]
      for (stat <- b.getStatements) {
        newStats.append(rewrite(stat))
        if (stat.isInstanceOf[LoopStatement]) {
          newStats.append(getBarrierInvocation)
        }
      }
      result = create.block(newStats.toList: _*)
    } else {
      super.visit(b)
    }
  }

}

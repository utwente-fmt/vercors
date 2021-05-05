package vct.col.veymont

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, LoopStatement}
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util._

class GenerateBarrier(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  override def visit(m : Method) = {
    if(m.getParent.isInstanceOf[ASTClass] && isThreadClassName(m.getParent.asInstanceOf[ASTClass].name)) {
      if (m.kind == Method.Kind.Constructor) {
        val newContract = new ContractBuilder()
        newContract.requires(create.expression(StandardOperator.NEQ, create.argument_name(getArgName(barrierFieldName)), create.reserved_name(ASTReserved.Null)))
        getBarrierAnnotations().foreach(newContract.ensures(_))
        rewrite(m.getContract, newContract)
        m.getBody match {
          case b: BlockStatement => {
            val newArgs = rewrite(m.getArgs) :+ create.field_decl(getArgName(barrierFieldName), getBarrierClass())
            val barAssign = create.assignment(create.field_name(barrierFieldName), create.argument_name(getArgName(barrierFieldName)))
            val newBody = create.block(rewrite(b.getStatements) :+ barAssign: _*)
            result = create.method_kind(m.kind, m.getReturnType, newContract.getContract, m.name, newArgs, newBody)
          }
          case _ => Fail("VeyMont Fail: expected BlockStatement in Method %s", m.name)
        }

      } else if (m.kind == Method.Kind.Pure || m.kind == Method.Kind.Predicate) {
        result = copy_rw.rewrite(m)
      } else {
        val newContract = new ContractBuilder()
        getBarrierAnnotations().foreach(newContract.context(_))
        rewrite(m.getContract, newContract)
        result = create.method_kind(m.kind, m.getReturnType, newContract.getContract, m.name, m.getArgs, rewrite(m.getBody))
      }
    } else {
      result = copy_rw.rewrite(m)
    }
  }

  override def visit(s : IfStatement) = {
    val stats : Seq[BlockStatement] = (0 until s.getCount).map(s.getStatement).filter {
      case b: BlockStatement => true
      case _ => Fail("VeyMont Fail: expected BlockStatement in IfStatementCase"); false
    }.asInstanceOf[Seq[BlockStatement]]
      .map(b => create.block(prependBarrier(b): _*))
    result = create.ifthenelse(rewrite(s.getGuard(0)),stats:_*)
  }

  override def visit(l : LoopStatement) = {
    l.getBody match {
      case b : BlockStatement => {
        val newContract = new ContractBuilder()
        getBarrierAnnotations().foreach(newContract.appendInvariant(_))
        rewrite(l.getContract,newContract)
        result = create.while_loop(rewrite(l.getEntryGuard),create.block(prependBarrier(b):_*),newContract.getContract)
      }
      case _ => Fail("VeyMont Fail: expected BlockStatement in LoopStatement")
    }
  }

  override def visit(b : BlockStatement) = {
    val nrLoops = b.getStatements.count(_.isInstanceOf[LoopStatement])
    if(nrLoops > 0) {
      val newStats = new Array[ASTNode](b.getLength+nrLoops)
      var i = 0
      for(stat <- b.getStatements) {
        newStats(i) = rewrite(stat)
        i = i+1
        if(stat.isInstanceOf[LoopStatement]) {
          newStats(i) = getBarrierInvokation()
          i = i+1
        }
      }
      result = create.block(newStats:_*)
    } else {
      super.visit(b)
    }
  }

  private def getBarrierInvokation() = create.invokation(create.field_name(barrierFieldName), null, barrierAwait)

  private def prependBarrier(b : BlockStatement) : Array[ASTNode] = getBarrierInvokation() +: rewrite(b.getStatements)

  private def getBarrierAnnotations() : List[OperatorExpression] = {
    List(create.expression(StandardOperator.Perm,create.field_name(barrierFieldName),create.reserved_name(ASTReserved.ReadPerm))
    ,create.expression(StandardOperator.NEQ,create.field_name(barrierFieldName),create.reserved_name(ASTReserved.Null)))
  }
}

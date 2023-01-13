package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.veymont.StructureCheck.VeyMontStructCheckError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object StructureCheck extends RewriterBuilder {

  override def key: String = "structureCheck"

  override def desc: String = "Check if program adheres to syntax of VeyMont input program"

  case class VeyMontStructCheckError(node : Node[_], msg: String) extends UserError { //SystemErrir fir unreachable errros
    override def code: String = "veyMontStructCheckError"

    override def text: String = node match {
      case Assign(_,_) => node.o.messageInContext("wong assignment" + msg)
    }
  }

}

case class StructureCheck[Pre <: Generation]() extends Rewriter[Pre] {

  val inRunMethod: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case dcl: RunMethod[Pre] => inRunMethod.having(()){rewriteDefault(dcl)}
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
    st match {
      case Assign(target,value) => rewriteDefault(st)//throw VeyMontStructCheckError(st,"bla")
      case other => rewriteDefault(other)
    }
  }

}

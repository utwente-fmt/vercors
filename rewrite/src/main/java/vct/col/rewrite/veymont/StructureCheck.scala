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

    override def text: String = node.o.messageInContext(msg)
  }

}

case class StructureCheck[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(prog : Program[Pre]) : Program[Post] = {
    if(!prog.declarations.exists {
      case dcl: VeyMontSeqProg[Pre] =>
        if(dcl.threads.isEmpty)
          throw VeyMontStructCheckError(dcl,"A seq_prog needs to have at least 1 thread, but none was found!")
        else true
      case _ => false
    })
      throw VeyMontStructCheckError(prog,"VeyMont requires a seq_prog, but none was found!")
    else rewriteDefault(prog)
  }



  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
    st match {
      case Assign(target,value) => //if inRunMethod.nonEmpty =>
        rewriteDefault(st)//throw VeyMontStructCheckError(st,"bla")
      case other => rewriteDefault(other)
    }
  }

//  object Linter {
//
//    def syntaxAllowed(a : AssignExpression[Pre]) : Boolean =
//      a.target match {
//        case Deref(obj, ref) => a.value match
//        case _ => false
//      }
//
//    def getDerefObj(Expr)
//  }

}

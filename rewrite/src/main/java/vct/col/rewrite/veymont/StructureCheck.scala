package vct.col.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.veymont.AddVeyMontAssignmentNodes.{getDerefsFromExpr,getThreadDeref}
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

  val inSeqProg: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case dcl: VeyMontSeqProg[Pre] => inSeqProg.having(()) {
        rewriteDefault(dcl)
      }
      case _ => rewriteDefault(decl)
    }

  override def dispatch(prog : Program[Pre]) : Program[Post] = {
    if(!prog.declarations.exists {
      case dcl: VeyMontSeqProg[Pre] =>
        if(dcl.threads.isEmpty)
          throw VeyMontStructCheckError(dcl,"A seq_program needs to have at least 1 thread, but none was found!")
        else true
      case _ => false
    })
      throw VeyMontStructCheckError(prog,"VeyMont requires a seq_program, but none was found!")
    else rewriteDefault(prog)
  }

  override def dispatch(st : Statement[Pre]) : Statement[Post] = {
    if(inSeqProg.nonEmpty)
      st match {
        case VeyMontCommExpression(_,_,_) => rewriteDefault(st)
        case VeyMontAssignExpression(_,_) => rewriteDefault (st)
        case Assign(_,_) => rewriteDefault (st)
        case Branch(_) => rewriteDefault(st)
        case Loop(_,_,_,_,_) => rewriteDefault(st)
        case Scope(_,_) => rewriteDefault(st)
        case Block(_) => rewriteDefault(st)
        case Eval(expr) => checkMethodCall(st, expr)
        case Assert(_) => rewriteDefault(st)
        case _ => throw VeyMontStructCheckError(st,"Statement not allowed in seq_program")
      }
    else rewriteDefault(st)
  }


  private def checkMethodCall(st: Statement[Pre], expr: Expr[Pre]): Statement[Post] = {
    expr match {
      case MethodInvocation(obj, _, args, _, _, _, _) => obj match {
        case ThisSeqProg(_) =>
          if (args.isEmpty) rewriteDefault(st)
          else throw VeyMontStructCheckError(st, "Calls to methods in seq_program cannot have any arguments!")
        case DerefVeyMontThread(thread) =>
          val argderefs = args.flatMap(getDerefsFromExpr)
          val argthreads = argderefs.map(d => getThreadDeref(d,
            VeyMontStructCheckError(st, "A method call on a thread object may only refer to a thread in its arguments!")))
          if (argthreads.forall(_ == thread.decl))
            rewriteDefault(st)
          else throw VeyMontStructCheckError(st, "A method call on a thread object may only refer to same thread in its arguments!")
        case _ => throw VeyMontStructCheckError(st, "This kind of method call is not allowed in seq_program")
      }
    }
  }
}

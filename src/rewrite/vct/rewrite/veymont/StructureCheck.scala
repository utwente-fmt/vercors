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
      case dcl: SeqProg[Pre] => inSeqProg.having(()) {
        rewriteDefault(dcl)
      }
      case m: InstanceMethod[Pre] =>
        if (inSeqProg.nonEmpty && m.args.nonEmpty)
          throw VeyMontStructCheckError(m, "Methods in seq_program cannot have any arguments!")
        else if(inSeqProg.nonEmpty && m.returnType != TVoid[Pre]())
          throw VeyMontStructCheckError(m, "Methods in seq_program cannot have a non-void return type!")
        else rewriteDefault(decl)
      case r: RunMethod[Pre] =>
        if(r.body.isEmpty)
          throw VeyMontStructCheckError(r, "Method run in seq_program needs to have a body!")
        else rewriteDefault(decl)
      case _ => rewriteDefault(decl)
    }

  override def dispatch(prog : Program[Pre]) : Program[Post] = {
    if(!prog.declarations.exists {
      case dcl: SeqProg[Pre] =>
        if(dcl.endpoints.isEmpty)
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
        case CommunicateX(_,_,_,_) => rewriteDefault(st)
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
        case EndpointUse(thread) =>
          val argderefs = args.flatMap(getDerefsFromExpr)
          val argthreads = argderefs.map(d => getThreadDeref(d,
            VeyMontStructCheckError(st, "A method call on a thread object may only refer to a thread in its arguments!")))
          if (argthreads.forall(_ == thread.decl))
            rewriteDefault(st)
          else throw VeyMontStructCheckError(st, "A method call on a thread object may only refer to same thread in its arguments!")
        case _ => throw VeyMontStructCheckError(st, "This kind of method call is not allowed in seq_program")
      }
      case _ => throw VeyMontStructCheckError(st, "This is not a method call")
    }
  }
}

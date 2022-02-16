package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.UserError

case object EncodeCurrentThread extends RewriterBuilder {
  case object CurrentThreadIdOrigin extends Origin {
    override def preferredName: String = "tid"
    override def context: String = "[At generated variable for the current thread ID]"
  }

  abstract class MisplacedCurrentThreadReference extends UserError {
    override def code: String = "curThreadScope"
  }

  case class MisplacedCurrentThreadConstant(node: CurrentThreadId[_]) extends MisplacedCurrentThreadReference {
    override def text: String =
      "This reference to \\current_thread is misplaced, since the surrounding declaration is not thread_local."
  }

  case class MisplacedThreadLocalInvocation(node: Apply[_]) extends MisplacedCurrentThreadReference {
    override def text: String =
      "This invocation refers to an applicable that is thread local, but the surrounding context is not thread local."
  }
}

case class EncodeCurrentThread[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeCurrentThread._

  val currentThreadId: ScopedStack[Expr[Post]] = ScopedStack()

  def wantsThreadLocal(app: Applicable[Pre]): Boolean = app match {
    case predicate: AbstractPredicate[Pre] => predicate.threadLocal
    case _: ContractApplicable[Pre] => true
    case _: ADTFunction[Pre] => false
    case _: ModelProcess[Pre] => false
    case _: ModelAction[Pre] => false
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: Applicable[Pre] =>
      if(wantsThreadLocal(app)) {
        val currentThreadVar = new Variable[Post](TInt())(CurrentThreadIdOrigin)
        currentThreadId.having(Local[Post](currentThreadVar.ref)(CurrentThreadIdOrigin)) {
          app.rewrite(args = collectInScope(variableScopes) {
            currentThreadVar.declareDefault(this)
            app.args.foreach(dispatch)
          }).succeedDefault(app)
        }
      } else {
        rewriteDefault(app)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case node @ CurrentThreadId() =>
      if(currentThreadId.isEmpty) {
        throw MisplacedCurrentThreadConstant(node)
      } else {
        currentThreadId.top
      }
    case apply: Apply[Pre] =>
      if(wantsThreadLocal(apply.ref.decl)) {
        if(currentThreadId.isEmpty) {
          throw MisplacedThreadLocalInvocation(apply)
        } else {
          apply.rewrite(args = currentThreadId.top +: apply.args.map(dispatch))
        }
      } else {
        apply.rewrite()
      }
    case other => rewriteDefault(other)
  }
}

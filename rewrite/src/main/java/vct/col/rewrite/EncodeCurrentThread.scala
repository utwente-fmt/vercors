package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object EncodeCurrentThread extends RewriterBuilder {
  override def key: String = "currentThread"
  override def desc: String = "Translate \\current_thread into an explicit argument to all thread-local contexts."

  case object CurrentThreadIdOrigin extends Origin {
    override def preferredName: String = "tid"
    override def shortPosition: String = "generated"
    override def context: String = "[At generated variable for the current thread ID]"
    override def inlineContext: String = "\\current_thread"
  }

  abstract class MisplacedCurrentThreadReference extends UserError {
    override def code: String = "curThreadScope"
  }

  case class MisplacedCurrentThreadConstant(node: CurrentThreadId[_]) extends MisplacedCurrentThreadReference {
    override def text: String =
      node.o.messageInContext("This reference to \\current_thread is misplaced, since the surrounding declaration is not thread_local.")
  }

  case class MisplacedThreadLocalInvocation(node: Apply[_]) extends MisplacedCurrentThreadReference {
    override def text: String =
      node.o.messageInContext("This invocation refers to an applicable that is thread local, but the surrounding context is not thread local.")
  }
}

case class EncodeCurrentThread[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeCurrentThread._

  val currentThreadId: ScopedStack[Expr[Post]] = ScopedStack()

  def wantsThreadLocal(app: Applicable[Pre]): Boolean = app match {
    case predicate: AbstractPredicate[Pre] => predicate.threadLocal
    case func: AbstractFunction[Pre] => func.threadLocal

    // PB: although a pure method will become a function, it should really be possible to mark a pure method as thread
    // local.
    case m: AbstractMethod[Pre] => !m.pure

    case _: ADTFunction[Pre] => false
    case _: ModelProcess[Pre] => false
    case _: ModelAction[Pre] => false
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: RunMethod[Pre] =>
      implicit val o: Origin = app.o
      classDeclarations.succeed(app, app.rewrite(body = app.body.map { body =>
        val currentThreadVar = new Variable[Post](TInt())(CurrentThreadIdOrigin)
        Scope(Seq(currentThreadVar), currentThreadId.having(currentThreadVar.get) { dispatch(body) })
      }))
    case app: Applicable[Pre] =>
      if(wantsThreadLocal(app)) {
        val currentThreadVar = new Variable[Post](TInt())(CurrentThreadIdOrigin)
        currentThreadId.having(Local[Post](currentThreadVar.ref)(CurrentThreadIdOrigin)) {
          allScopes.anySucceedOnly(app, allScopes.anyDeclare(app.rewrite(args = variables.collect {
            variables.declare(currentThreadVar)
            app.args.foreach(dispatch)
          }._1)))
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

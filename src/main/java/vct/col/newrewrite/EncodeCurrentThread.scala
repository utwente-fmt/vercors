package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.UserError

case object EncodeCurrentThread {
  case object CurrentThreadIdOrigin extends Origin {
    override def preferredName: String = "tid"
    override def messageInContext(message: String): String =
      s"[At generated variable for the current thread ID]: $message"
  }

  case class MisplacedCurrentThreadReference(node: CurrentThreadId) extends UserError {
    override def code: String = "curThreadScope"
    override def text: String = node.o.messageInContext(
      "This reference to \\current_thread is misplaced, since the surrounding declaration is not thread_local.")
  }
}

case class EncodeCurrentThread() extends Rewriter {
  import EncodeCurrentThread._

  val currentThreadId: ScopedStack[Expr] = ScopedStack()

  def wantsThreadLocal(app: Applicable): Boolean = app match {
    case predicate: AbstractPredicate => predicate.threadLocal
    case _: ContractApplicable => true
    case _: ADTFunction => false
    case _: ModelProcess => false
    case _: ModelAction => false
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case app: Applicable =>
      if(wantsThreadLocal(app)) {
        val currentThreadVar = new Variable(TInt())(CurrentThreadIdOrigin)
        currentThreadId.having(Local(currentThreadVar.ref)(CurrentThreadIdOrigin)) {
          app.rewrite(args = collectInScope(variableScopes) {
            currentThreadVar.declareDefault(this)
            app.args.foreach(dispatch)
          })
        }
      } else {
        rewriteDefault(app)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case node @ CurrentThreadId() =>
      if(currentThreadId.isEmpty) {
        throw MisplacedCurrentThreadReference(node)
      } else {
        currentThreadId.top
      }
  }
}

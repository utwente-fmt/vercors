package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, PreferredName}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object EncodeCurrentThread extends RewriterBuilder {
  override def key: String = "currentThread"
  override def desc: String =
    "Translate \\current_thread into an explicit argument to all thread-local contexts."

  private val currentThreadIdOrigin: Origin = Origin(
    Seq(PreferredName(Seq("tid")), LabelContext("\\current_thread"))
  )

  abstract class MisplacedCurrentThreadReference extends UserError {
    override def code: String = "curThreadScope"
  }

  case class MisplacedCurrentThreadConstant(node: CurrentThreadId[_])
      extends MisplacedCurrentThreadReference {
    override def text: String =
      node.o.messageInContext(
        "This reference to \\current_thread is misplaced, since the surrounding declaration is not thread_local."
      )
  }

  case class MisplacedThreadLocalInvocation(node: Node[_])
      extends MisplacedCurrentThreadReference {
    override def text: String =
      node.o.messageInContext(
        "This invocation refers to an applicable that is thread local, but the surrounding context is not thread local."
      )
  }
}

case class EncodeCurrentThread[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeCurrentThread._

  val currentThreadId: ScopedStack[Expr[Post]] = ScopedStack()

  def wantsThreadLocal(app: Applicable[Pre]): Boolean =
    app match {
      case predicate: AbstractPredicate[Pre] => predicate.threadLocal
      case func: AbstractFunction[Pre] => func.threadLocal

      // PB: although a pure method will become a function, it should really be possible to mark a pure method as thread
      // local.
      case m: AbstractMethod[Pre] => !m.pure
      case m: LLVMFunctionDefinition[Pre] => !m.pure

      case _: ADTFunction[Pre] => false
      case _: ProverFunction[Pre] => false
      case _: ModelProcess[Pre] => false
      case _: ModelAction[Pre] => false
    }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case app: RunMethod[Pre] =>
        implicit val o: Origin = app.o
        classDeclarations.succeed(
          app,
          app.rewrite(body = app.body.map { body =>
            val currentThreadVar =
              new Variable[Post](TInt())(currentThreadIdOrigin)
            Scope(
              Seq(currentThreadVar),
              currentThreadId.having(currentThreadVar.get) { dispatch(body) },
            )
          }),
        )
      case chor: Choreography[Pre] =>
        implicit val o = chor.o
        // Assume the whole choreography gets analyzed in the context of one "currentThread".
        // If someone makes choreographies callable in the future, they can decide if they
        // also want distinct currentThread values for each endpoint.
        val currentThreadVar = new Variable[Post](TInt())(currentThreadIdOrigin)
        currentThreadId.having(currentThreadVar.get) {
          chor.rewrite(params =
            variables.collect {
              currentThreadVar.declare()
              chor.params.foreach(dispatch)
            }._1
          ).succeed(chor)
        }
      case app: Applicable[Pre] =>
        if (wantsThreadLocal(app)) {
          val currentThreadVar =
            new Variable[Post](TInt())(currentThreadIdOrigin)
          currentThreadId
            .having(Local[Post](currentThreadVar.ref)(currentThreadIdOrigin)) {
              allScopes.anySucceedOnly(
                app,
                allScopes.anyDeclare(app.rewrite(args =
                  variables.collect {
                    variables.declare(currentThreadVar)
                    app.args.foreach(dispatch)
                  }._1
                )),
              )
            }
        } else { rewriteDefault(app) }
      case other => rewriteDefault(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case node @ CurrentThreadId() =>
        if (currentThreadId.isEmpty) {
          throw MisplacedCurrentThreadConstant(node)
        } else { currentThreadId.top }
      case apply: Apply[Pre] =>
        if (wantsThreadLocal(apply.ref.decl)) {
          if (currentThreadId.isEmpty) {
            throw MisplacedThreadLocalInvocation(apply)
          } else {
            apply
              .rewrite(args = currentThreadId.top +: apply.args.map(dispatch))
          }
        } else { apply.rewrite() }
      case other => rewriteDefault(other)
    }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] =
    stat match {
      case invoke: InvocationStatement[Pre] =>
        if (wantsThreadLocal(invoke.ref.decl)) {
          if (currentThreadId.isEmpty) {
            throw MisplacedThreadLocalInvocation(invoke)
          } else {
            invoke
              .rewrite(args = currentThreadId.top +: invoke.args.map(dispatch))
          }
        } else { invoke.rewrite() }
      case other => rewriteDefault(other)
    }
}

package vct.col.newrewrite.exc

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter

case class EncodeTryThrowSignals() extends Rewriter {
  val currentException: ScopedStack[Variable] = ScopedStack()
  val exceptionalHandlerEntry: ScopedStack[LabelDecl] = ScopedStack()

  def getExc(implicit o: Origin): Expr =
    currentException.last.get

  override def dispatch(stat: Statement): Statement = {
    implicit val o: Origin = stat.o
    stat match {
      case TryCatchFinally(body, after, catches) =>
        val handlersEntry = new LabelDecl()
        val finallyEntry = new LabelDecl()

        val newBody = exceptionalHandlerEntry.having(handlersEntry) {
          dispatch(body)
        }

        val catchImpl = Block(catches.map {
          case CatchClause(decl, body) =>
            Branch(Seq((
              getExc !== Null() && InstanceOf(getExc, TypeValue(decl.t)),
              Block(Seq(
                exceptionalHandlerEntry.having(finallyEntry) {
                  dispatch(
                    Substitute(Map(decl.get -> getExc)).dispatch(body)
                  )
                },
                Assign(getExc, Null()),
              ),
            ))))
        })

        val finallyImpl = Block(Seq(
          Label(finallyEntry, Block(Nil)),
          dispatch(after),
          Branch(Seq((
            getExc !== Null(),
            Goto(exceptionalHandlerEntry.last.ref),
          ))),
        ))

        Block(Seq(
          newBody,
          Label(handlersEntry, Block(Nil)),
          catchImpl,
          finallyImpl,
        ))
    }
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case method: AbstractMethod =>
      implicit val o: Origin = method.o
      method.rewrite(body = method.body.map(body => {
        val exc = new Variable(TClass(???))
        // Before dispatching to body we should push an exceptionalHandlerEntry, to deal with an exception bubbling
        // up the stack.
        Scope(Seq(exc), Block(Seq(
          Assign(exc.get, Null()),
          currentException.having(exc) { dispatch(body) },
        )))
      }))
  }
}

package vct.col.newrewrite.exc

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.{AssertFailed, Blame, FramedGetLeft, FramedGetRight, Origin, ThrowNull}
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers

case object EncodeTryThrowSignals {
  case class ThrowNullAssertFailed(t: Throw) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      t.blame.blame(ThrowNull(t))
  }
}

case class EncodeTryThrowSignals() extends Rewriter {
  import EncodeTryThrowSignals._

  val currentException: ScopedStack[Variable] = ScopedStack()
  val exceptionalHandlerEntry: ScopedStack[LabelDecl] = ScopedStack()
  val returnHandler: ScopedStack[LabelDecl] = ScopedStack()

  val signalsBinding: ScopedStack[(Variable, Result)] = ScopedStack()

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

      case t @ Throw(obj) =>
        Block(Seq(
          Assign(getExc, dispatch(obj)),
          Assert(getExc !== Null())(ThrowNullAssertFailed(t)),
          Goto(exceptionalHandlerEntry.last.ref),
        ))

      case Return(result) =>
        Block(Seq(
          Label(returnHandler.last, Block(Nil)),
          Return(Select(
            getExc === Null(),
            EitherRight(dispatch(result)),
            EitherLeft(getExc),
          ))
        ))

      case other => rewriteDefault(other)
    }
  }

  override def dispatch(decl: Declaration): Unit = decl match {
    case method: AbstractMethod =>
      implicit val o: Origin = method.o

      val exc = new Variable(TAny()) // PB TODO: TClass(?)

      currentException.having(exc) {
        val body = method.body.map(body => {
          val returnPoint = new LabelDecl()
          // The return point is at this moment guaranteed to be the last logical statement of the method, since either
          // all return statements are encoded as a goto to the end of the method, or they are encoded as exceptions.
          // Because of that, we can designate the (only) return statement as the outermost exception handler, returning
          // Left(exc) if an exception is being handled, Right(result) otherwise.
          // However, we should figure out a better way of doing this. Perhaps first translating to out-parameters is
          // the way to go? But then we should perhaps add an out-parameter option<exception> or so, instead of changing
          // the return type to either<exc, _>.
          Scope(Seq(exc), Block(Seq(
            Assign(exc.get, Null()),
            returnHandler.having(returnPoint) {
              exceptionalHandlerEntry.having(returnPoint) {
                currentException.having(exc) {
                  dispatch(body)
                }
              }
            },
          )))
        })

        val ensures =
          (IsRight(Result(succ(method))) ==> dispatch(method.contract.ensures)) &*
            AstBuildHelpers.foldStar(method.contract.signals.map {
              case SignalsClause(binding, assn) =>
                binding.drop()
                (IsLeft(Result(succ(method))) && InstanceOf(GetLeft(Result(succ(method)))(FramedGetLeft), TypeValue(binding.t))) ==>
                  (signalsBinding.having((binding, Result(succ(method)))) { dispatch(assn) })
            })

        method.rewrite(
          returnType = TEither(TAny(), dispatch(method.returnType)),
          body = body,
          contract = method.contract.rewrite(ensures = ensures, signals = Nil)
        ).succeedDefault(this, method)
      }

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case Local(Ref(v)) if signalsBinding.nonEmpty && signalsBinding.last._1 == v =>
      implicit val o: Origin = e.o
      GetLeft(signalsBinding.last._2)(FramedGetLeft)

    case Result(Ref(app)) =>
      implicit val o: Origin = e.o
      app match {
        case _: AbstractMethod => GetRight(Result(succ(app)))(FramedGetRight)
        case _ => Result(succ(app))
      }

    case other => rewriteDefault(other)
  }
}

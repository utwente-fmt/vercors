package vct.col.rewrite.exc

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable

import scala.collection.mutable

case object EncodeTryThrowSignals extends RewriterBuilder {
  override def key: String = "tryThrowSignals"

  override def desc: String = "Encode try, throw and signals specifications to goto, exception out-parameters and regular postconditions."

  case class ThrowNullAssertFailed(t: Throw[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      t.blame.blame(ThrowNull(t))
  }

  case class PackageThrowsAssertFailed(pack: WandPackage[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit = pack.blame.blame(PackageThrows(pack))
  }

  private def ExcVar: Origin = Origin(
    Seq(
      PreferredName(Seq("exc")),
      LabelContext("current exception"),
    )
  )

  private def CurrentlyHandling: Origin = Origin(
    Seq(
      PreferredName(Seq("currently_handling_exc")),
      LabelContext("handling exception"),
    )
  )

  private def ReturnPoint: Origin = Origin(
    Seq(
      PreferredName(Seq("bubble")),
      LabelContext("bubble label"),
    )
  )

  private def CatchLabel: Origin = Origin(
    Seq(
      PreferredName(Seq("catches")),
      LabelContext("catch label"),
    )
  )

  private def FinallyLabel: Origin = Origin(
    Seq(
      PreferredName(Seq("finally")),
      LabelContext("finally label"),
    )
  )

  private def ExcBeforeLoop: Origin = Origin(
    Seq(
      PreferredName(Seq("excBeforeLoop")),
      LabelContext("loop exception"),
    )
  )

  case class SignalsClosedPostconditionFailed(method: AbstractMethod[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      method.blame.blame(ExceptionNotInSignals(method))
  }

  case class AssertFailedSignalsNotClosed(method: AbstractMethod[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      method.blame.blame(ExceptionNotInSignals(method))
  }

  case class SignalsFailedPostconditionFailed(method: AbstractMethod[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      method.blame.blame(SignalsFailed(error.failure, method))
  }
}

case class EncodeTryThrowSignals[Pre <: Generation]() extends Rewriter[Pre] {

  import EncodeTryThrowSignals._

  val currentException: ScopedStack[Variable[Post]] = ScopedStack()
  val exceptionalHandlerEntry: ScopedStack[LabelDecl[Post]] = ScopedStack()
  val returnHandler: ScopedStack[LabelDecl[Post]] = ScopedStack()


  val needCurrentExceptionRestoration: ScopedStack[Boolean] = ScopedStack()
  needCurrentExceptionRestoration.push(false)

  val signalsBinding: ScopedStack[(Variable[Pre], Expr[Post])] = ScopedStack()
  val catchBindings: mutable.Set[Variable[Pre]] = mutable.Set()

  def getExc(implicit o: Origin): Local[Post] =
    currentException.top.get

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = stat.o
    stat match {
      case TryCatchFinally(body, after, catches) =>
        val handlersEntry = new LabelDecl[Post]()(CatchLabel)
        val finallyEntry = new LabelDecl[Post]()(FinallyLabel)

        val newBody = exceptionalHandlerEntry.having(handlersEntry) {
          needCurrentExceptionRestoration.having(false) {
            dispatch(body)
          }
        }

        val catchImpl = Block[Post](catches.map {
          case CatchClause(decl, body) =>
            val typedExc = variables.dispatch(decl)
            Scope(Seq(typedExc), Branch(Seq((
              (getExc !== Null[Post]()) && InstanceOf(getExc, TypeValue(dispatch(decl.t))),
              Block(Seq(
                assignLocal(typedExc.get, Cast(getExc, TypeValue(dispatch(decl.t)))),
                assignLocal(getExc, Null()),
                exceptionalHandlerEntry.having(finallyEntry) {
                  needCurrentExceptionRestoration.having(true) {
                    dispatch(body)
                  }
                },
              ),
              )))))
        })

        val finallyImpl = Block[Post](Seq(
          Label(finallyEntry, Block(Nil)),
          needCurrentExceptionRestoration.having(true) {
            dispatch(after)
          },
          Branch(Seq((
            getExc !== Null(),
            Goto(exceptionalHandlerEntry.top.ref),
          ))),
        ))

        val (store: Statement[Post], restore: Statement[Post], vars: Seq[Variable[Post]]) = if (needCurrentExceptionRestoration.top) {
          val tmp = new Variable[Post](TAnyClass())(CurrentlyHandling)
          (
            Block[Post](Seq(
              assignLocal(tmp.get, getExc),
              assignLocal(getExc, Null()),
            )),
            assignLocal[Post](getExc, tmp.get),
            Seq(tmp),
          )
        } else (Block[Post](Nil), Block[Post](Nil), Nil)

        Scope(vars, Block(Seq(
          store,
          newBody,
          Label(handlersEntry, Block(Nil)),
          catchImpl,
          finallyImpl,
          restore,
        )))

      case t@Throw(obj) =>
        Block(Seq(
          assignLocal(getExc, dispatch(obj)),
          Assert(getExc !== Null())(ThrowNullAssertFailed(t)),
          Goto(exceptionalHandlerEntry.top.ref),
        ))

      case inv: InvokeProcedure[Pre] if inv.ref.decl.contract.signals.isEmpty =>
        rewriteDefault(inv)

      case inv: InvokeProcedure[Pre] =>
        Block(Seq(
          inv.rewrite(outArgs = currentException.top.get +: inv.outArgs.map(dispatch)),
          Branch(Seq((
            getExc !== Null(),
            Goto(exceptionalHandlerEntry.top.ref),
          ))),
        ))

      case inv: InvokeMethod[Pre] if inv.ref.decl.contract.signals.isEmpty =>
        rewriteDefault(inv)

      case inv: InvokeMethod[Pre] =>
        Block(Seq(
          inv.rewrite(outArgs = currentException.top.get +: inv.outArgs.map(dispatch)),
          Branch(Seq((
            getExc !== Null(),
            Goto(exceptionalHandlerEntry.top.ref),
          ))),
        ))

      case loop: Loop[Pre] =>
        val beforeLoop = new Variable[Post](TAnyClass())(ExcBeforeLoop)

        Scope(Seq(beforeLoop), Block[Post](Seq(
          assignLocal(beforeLoop.get, getExc),
          loop.rewrite(contract = loop.contract match {
            case inv@LoopInvariant(invariant, decreases) =>
              LoopInvariant(getExc === beforeLoop.get &* dispatch(invariant), decreases.map(dispatch))(inv.blame)
            case it: IterationContract[Pre] => rewriteDefault(it)
          })
        )))

      case w@WandPackage(wand, proof) =>
        val exc = new Variable[Post](TAnyClass())(ExcVar)
        val labelHandler = new LabelDecl[Post]()
        val labelDone = new LabelDecl[Post]()
        exceptionalHandlerEntry.having(labelHandler) {
          currentException.having(exc) {
            WandPackage(dispatch(wand),
              Scope(Seq(exc),
                Block(
                  Seq(
                    dispatch(proof),
                    Goto[Post](labelDone.ref),
                    Label(labelHandler, Block(Nil)),
                    Assert[Post](BooleanValue(false))(PackageThrowsAssertFailed(w)),
                    Label(labelDone, Block(Nil)),
                  ))
              )
            )(w.blame)
          }
        }

      case other => rewriteDefault(other)
    }
  }

  def inlineExtraCondition(condition: Expr[Post], clause: AccountedPredicate[Pre]): AccountedPredicate[Post] = clause match {
    case UnitAccountedPredicate(pred) =>
      UnitAccountedPredicate[Post](mapUnfoldedStar(pred, (e: Expr[Pre]) => Implies(condition, dispatch(e))(e.o)))(clause.o)
    case SplitAccountedPredicate(left, right) =>
      SplitAccountedPredicate[Post](inlineExtraCondition(condition, left), inlineExtraCondition(condition, right))(clause.o)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] if method.contract.signals.isEmpty =>
      implicit val o: Origin = method.o

      val exc = new Variable[Post](TAnyClass())(ExcVar)

      currentException.having(exc) {
        lazy val body = method.body.map(body => {
          val bubble = new LabelDecl[Post]()(ReturnPoint)

          Scope(Seq(exc), Block(Seq(
            assignLocal(exc.get, Null()),
            exceptionalHandlerEntry.having(bubble) {
              currentException.having(exc) {
                dispatch(body)
              }
            },
            Label(bubble, Block(Nil)),
            Assert(exc.get === Null())(AssertFailedSignalsNotClosed(method))
          )))
        })

        allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(
          body = body,
        )))
      }

    case method: AbstractMethod[Pre] =>
      implicit val o: Origin = method.o

      val exc = new Variable[Post](TAnyClass())(ExcVar)

      currentException.having(exc) {
        lazy val body = method.body.map(body => {
          val bubble = new LabelDecl[Post]()(ReturnPoint)

          Block(Seq(
            assignLocal(exc.get, Null()),
            exceptionalHandlerEntry.having(bubble) {
              currentException.having(exc) {
                dispatch(body)
              }
            },
            Label(bubble, Block(Nil)),
          ))
        })

        lazy val ensures: AccountedPredicate[Post] = SplitAccountedPredicate(
          left = UnitAccountedPredicate((exc.get !== Null()) ==> foldOr(method.contract.signals.map {
            case SignalsClause(binding, _) => InstanceOf(exc.get, TypeValue(dispatch(binding.t)))
          })),
          right = SplitAccountedPredicate(
            left = inlineExtraCondition(exc.get === Null(), method.contract.ensures),
            right = UnitAccountedPredicate(AstBuildHelpers.foldStar(method.contract.signals.map {
              case SignalsClause(binding, assn) =>
                implicit val o: Origin = assn.o
                binding.drop()
                ((exc.get !== Null()) && InstanceOf(exc.get, TypeValue(dispatch(binding.t)))) ==>
                  signalsBinding.having((binding, exc.get)) {
                    dispatch(assn)
                  }
            })),
          ),
        )

        allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(
          blame = PostBlameSplit.left(
            left = SignalsClosedPostconditionFailed(method),
            right = PostBlameSplit.right(
              left = method.blame,
              right = SignalsFailedPostconditionFailed(method),
            ),
          ),
          body = body,
          outArgs = variables.collect { variables.declare(exc); method.outArgs.foreach(dispatch) }._1,
          contract = method.contract.rewrite(ensures = ensures, signals = Nil),
        )))
      }

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Local(Ref(v)) if signalsBinding.nonEmpty && signalsBinding.top._1 == v =>
      implicit val o: Origin = e.o
      signalsBinding.top._2

    case other => rewriteDefault(other)
  }
}

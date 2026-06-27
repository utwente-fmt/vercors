package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.CheckPostconditionSatisfiability.{
  AssertPassedPostconditionUnsatisfiable,
  NotWellFormedIgnoreCheckPostSat,
}
import vct.col.rewrite.util.{Extract, WellDefinednessConditions}
import vct.col.origin.{
  Blame,
  ExpectedError,
  ExpectedErrorFailure,
  ExpectedErrorNotTripped,
  ExpectedErrorTrippedTwice,
  FilterExpectedErrorBlame,
  NontrivialPostconditionUnsatisfiable,
  Origin,
  PanicBlame,
  UnsafeDontCare,
  VerificationFailure,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}

import vct.col.util.AstBuildHelpers.{
  ExprBuildHelpers,
  ff,
  foldStar,
  procedure,
  tt,
}
import vct.col.util.Substitute

import scala.collection.mutable.ArrayBuffer

// Checks that method postconditions are satisfiable given their preconditions.
// For each applicable, generates a standalone procedure that inhales the preconditions (non-heap)
// and postcondition, then asserts false. If that assert passes (the postcondition
// is contradictory), postUnsatisfiable is reported.
case object CheckPostconditionSatisfiability
    extends RewriterBuilderArg[Boolean] {
  override def key: String = "checkPostSat"
  override def desc: String =
    "Prove that postconditions are not internally contradictory (i.e. unsatisfiable) given the preconditions."

  case class AssertPassedPostconditionUnsatisfiable(
      contract: ApplicableContract[_],
      applicable: ContractApplicable[_],
  ) extends Blame[ExpectedErrorFailure] {
    override def blame(error: ExpectedErrorFailure): Unit =
      error match {
        case _: ExpectedErrorTrippedTwice =>
        case ExpectedErrorNotTripped(_) =>
          applicable.blame.blame(NontrivialPostconditionUnsatisfiable(contract))
      }
  }

  case class NotWellFormedIgnoreCheckPostSat(err: ExpectedError)
      extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = err.trip(error)
  }
}

case class CheckPostconditionSatisfiability[Pre <: Generation](
    doCheck: Boolean = true
) extends Rewriter[Pre] {
  import CheckPostconditionSatisfiability._

  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack()
  val wellFormednessBlame: ScopedStack[Blame[VerificationFailure]] =
    ScopedStack()
  val currentApplicable: ScopedStack[ContractApplicable[Pre]] = ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    wellFormednessBlame.topOption.getOrElse(blame)

  private def returnTypeOf(ca: ContractApplicable[Pre]): Option[Type[Pre]] =
    ca match {
      case p: Procedure[Pre] => Some(p.returnType).filterNot(_ == TVoid[Pre]())
      case f: AbstractFunction[Pre] => Some(f.returnType)
      case _ => None
    }

  private def substituteResult(
      expr: Expr[Pre],
      applicable: ContractApplicable[Pre],
      resultVar: Variable[Pre],
  )(implicit o: Origin): Expr[Pre] =
    Substitute[Pre](Map(
      Result[Pre](applicable.ref)(o) -> Local[Pre](resultVar.ref)(o)
    )).dispatch(expr)

  private def eliminateOldInTriggers(expr: Expr[Pre]): Expr[Pre] = {
    val triggerOlds: Set[Old[Pre]] =
      expr.flatCollect {
        case q: Forall[Pre] =>
          q.triggers.flatten.collect { case old: Old[Pre] => old }
        case q: Starall[Pre] =>
          q.triggers.flatten.collect { case old: Old[Pre] => old }
        case q: Exists[Pre] =>
          q.triggers.flatten.collect { case old: Old[Pre] => old }
      }.toSet
    if (triggerOlds.isEmpty)
      return expr
    Substitute[Pre](triggerOlds.map { old => (old: Expr[Pre]) -> old.expr }
      .toMap).dispatch(expr)
  }

  def checkPostcondition(
      contract: ApplicableContract[Pre],
      applicable: ContractApplicable[Pre],
  ): Unit = {
    implicit val origin: Origin = applicable.o.where(prefix = "checkPostSat")

    val ensuresPred = foldStar(
      WellDefinednessConditions.splitPred(contract.ensures)
    )
    ensuresPred match {
      case BooleanValue(
            false
          ) => // trivially false postcondition — skip (already always an error)
      case BooleanValue(true) => // trivially true — nothing to check
      case _ =>
        val postPredAfterResult =
          returnTypeOf(applicable) match {
            case Some(rt) =>
              val v = new Variable[Pre](rt)(origin.where(name = "result"))
              substituteResult(ensuresPred, applicable, v)
            case None => ensuresPred
          }
        val postPred = eliminateOldInTriggers(postPredAfterResult)

        val err = ExpectedError(
          "assertFailed:false",
          origin,
          AssertPassedPostconditionUnsatisfiable(contract, applicable),
        )
        val onlyAssertBlame = FilterExpectedErrorBlame(
          PanicBlame("A boolean assert can only report assertFailed:false"),
          err,
        )
        expectedErrors.top += err
        val combined = postPred

        val nonHeapPreConds = WellDefinednessConditions
          .splitPred(contract.requires)
          .flatMap(WellDefinednessConditions.extractNonHeap)

        val extractObj = Extract[Pre]()
        val extracted = extractObj.extract(combined)
        val extractedPreConds = nonHeapPreConds.map(extractObj.extract)
        val extractObj.Data(ts, in, _, _, _) = extractObj.finish()
        variables.scope {
          localHeapVariables.scope {
            globalDeclarations.declare(procedure(
              blame = PanicBlame(
                "The postcondition of a method checking post-sat is empty"
              ),
              contractBlame = UnsafeDontCare.Satisfiability(
                "the precondition of a check-post-sat method is only there to check it."
              ),
              requires = UnitAccountedPredicate(tt)(extracted.o),
              typeArgs = variables.dispatch(ts.keys),
              args = variables.dispatch(in.keys),
              body = Some(Scope[Post](
                Nil,
                Block(
                  extractedPreConds.map(pc => Inhale(dispatch(pc))) ++ Seq(
                    Inhale(
                      wellFormednessBlame
                        .having(NotWellFormedIgnoreCheckPostSat(err)) {
                          dispatch(extracted)
                        }
                    ),
                    Assert(ff)(onlyAssertBlame),
                  )
                ),
              )),
            ))
          }
        }
    }
  }

  override def dispatch(verification: Verification[Pre]): Verification[Post] = {
    val (errs, tasks) = expectedErrors.collect {
      verification.tasks.map(dispatch)
    }
    Verification(tasks, errs ++ verification.expectedErrors)(verification.o)
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case ca: ContractApplicable[Pre] =>
        currentApplicable.having(ca) { super.dispatch(decl) }
      case _ => super.dispatch(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] = {
    if (doCheck)
      currentApplicable.topOption.foreach(checkPostcondition(contract, _))
    rewriteDefault(contract)
  }
}

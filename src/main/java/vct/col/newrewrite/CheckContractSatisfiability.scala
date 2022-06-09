package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.CheckContractSatisfiability.{AssertPassedNontrivialUnsatisfiable, CheckSatOrigin}
import vct.col.newrewrite.util.Extract
import vct.col.origin.{Blame, ExpectedErrorFailure, ExpectedErrorNotTripped, ExpectedErrorTrippedTwice, FilterExpectedErrorBlame, NontrivialUnsatisfiable, Origin, PanicBlame, UnsafeDontCare}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.{ff, foldStar, procedure, unfoldStar}
import vct.col.util.ExpectedError

import scala.collection.mutable.ArrayBuffer

case object CheckContractSatisfiability extends RewriterBuilder {
  override def key: String = "checkSat"
  override def desc: String =
    "Prove that contracts are not internally contradictory (i.e. unsatisfiable) for methods and other contract bearers, " +
      "except for the contract `false`."

  case class CheckSatOrigin(inner: Origin) extends Origin {
    override def preferredName: String = "check_sat_" + inner.preferredName
    override def context: String = inner.context
    override def inlineContext: String = inner.inlineContext
    override def shortPosition: String = inner.shortPosition
  }

  case class AssertPassedNontrivialUnsatisfiable(contract: ApplicableContract[_]) extends Blame[ExpectedErrorFailure] {
    override def blame(error: ExpectedErrorFailure): Unit = error match {
      case err: ExpectedErrorTrippedTwice =>
        PanicBlame("A single assert cannot trip twice").blame(err)
      case ExpectedErrorNotTripped(_) =>
        contract.blame.blame(NontrivialUnsatisfiable(contract))
    }
  }
}

case class CheckContractSatisfiability[Pre <: Generation]() extends Rewriter[Pre] {
  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack[ArrayBuffer[ExpectedError]]()

  def splitAccountedPredicate(pred: AccountedPredicate[Pre]): Seq[Expr[Pre]] = pred match {
    case UnitAccountedPredicate(pred) => unfoldStar(pred)
    case SplitAccountedPredicate(left, right) => splitAccountedPredicate(left) ++ splitAccountedPredicate(right)
  }

  def checkSatisfiability(contract: ApplicableContract[Pre]): Unit = {
    implicit val origin: Origin = CheckSatOrigin(contract.o)
    foldStar(splitAccountedPredicate(contract.requires)) match {
      case BooleanValue(false) =>
        // Assume the contract is not intended to be satisfiable
      case pred =>
        val err = ExpectedError("assertFailed:false", origin, AssertPassedNontrivialUnsatisfiable(contract))
        val onlyAssertBlame = FilterExpectedErrorBlame(PanicBlame("A boolean assert can only report assertFailed:false"), err)
        expectedErrors.top += err
        val (Seq(generalizedContract), substitutions) = Extract.extract(pred)
        procedure(
          blame = PanicBlame("The postcondition of a method checking satisfiability is empty"),
          contractBlame = UnsafeDontCare.Satisfiability("the precondition of a check-sat method is only there to check it."),
          requires = UnitAccountedPredicate(dispatch(generalizedContract))(generalizedContract.o),
          args = collectInScope(variableScopes) { substitutions.keys.foreach(dispatch) },
          body = Some(Scope[Post](Nil, Assert(ff)(onlyAssertBlame)))
        ).declareDefault(this)
    }
  }

  override def dispatch(context: VerificationContext[Pre]): VerificationContext[Post] = {
    val (errs, program) = withCollectInScope(expectedErrors) {
      dispatch(context.program)
    }
    // PB: Important: the expected errors from this pass must appear before other errors, since the absence of an assert
    // failure from this pass may in turn indicate an expected "unsatisfiable" error from an earlier pass.
    VerificationContext(program, errs ++ context.expectedErrors)(context.o)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = {
    checkSatisfiability(contract)
    rewriteDefault(contract)
  }
}
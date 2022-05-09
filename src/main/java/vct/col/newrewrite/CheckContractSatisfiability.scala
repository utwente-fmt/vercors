package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.CheckContractSatisfiability.CheckSatOrigin
import vct.col.newrewrite.util.Extract
import vct.col.origin.{FilterExpectedErrorBlame, Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.{ff, procedure}
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
}

case class CheckContractSatisfiability[Pre <: Generation]() extends Rewriter[Pre] {
  val expectedErrors: ScopedStack[ArrayBuffer[ExpectedError]] = ScopedStack[ArrayBuffer[ExpectedError]]()

  def checkSatisfiability(contract: Expr[Pre], o: Origin): Unit =
    contract match {
      case BooleanValue(false) =>
        // Assume the contract is not intended to be satisfiable
      case contract =>
        implicit val origin: Origin = CheckSatOrigin(o)
        val err = ExpectedError("assertFailed:false", origin, ???)
        expectedErrors.top += err
        val (Seq(generalizedContract), substitutions) = Extract.extract(contract)
        procedure(
          blame = PanicBlame("The postcondition of a method checking satisfiability is empty"),
          requires = UnitAccountedPredicate(dispatch(generalizedContract))(generalizedContract.o),
          args = collectInScope(variableScopes) { substitutions.keys.foreach(dispatch) },
          body = Some(Scope[Post](Nil, Assert(ff)(FilterExpectedErrorBlame(???, err))))
        )(CheckSatOrigin(o)).declareDefault(this)
    }

  override def dispatch(context: VerificationContext[Pre]): VerificationContext[Post] = {
    val (errs, program) = withCollectInScope(expectedErrors) {
      dispatch(context.program)
    }
    VerificationContext(program, context.expectedErrors ++ errs)(context.o)
  }
}
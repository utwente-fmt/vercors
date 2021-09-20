package viper.api

import org.scalatest.Assertions.fail
import vct.col.ast.{Assert, Blame, ContractApplicable, ContractFailure, Deref, DividingExpr, Exhale, Invocation, Old, SeqSubscript, SilverDeref, SilverFieldAssign, SilverFold, SilverUnfold, SilverWhile, VerificationFailure}

import scala.collection.mutable

case object NoErrors extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit =
    fail(error.toString)
}

class ExpectedErrorsRegistry {
  val errors: mutable.Set[ExpectError] = mutable.Set()

  def check(): Unit =
    errors.foreach(e => assert(e.tripped, "An expected error was not raised."))
}

case class ExpectError()(implicit val registry: ExpectedErrorsRegistry) extends Blame[VerificationFailure] {
  var tripped = false
  registry.errors += this

  override def blame(error: VerificationFailure): Unit = tripped = true
}
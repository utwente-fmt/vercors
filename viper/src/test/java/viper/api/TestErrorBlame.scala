package viper.api

import org.scalatest.Assertions.fail
import vct.col.ast.{Assert, ContractApplicable, ContractFailure, Deref, DividingExpr, Exhale, Invocation, Old, Scapegoat, SeqSubscript, SilverDeref, SilverFieldAssign, SilverFold, SilverUnfold, SilverWhile}

import scala.collection.mutable

class NoErrors extends Scapegoat {
  override def silverAssignFailed(assign: SilverFieldAssign): Unit =
    fail(s"There must be sufficient permission to assign $assign")
  override def silverInsufficientPermission(deref: SilverDeref): Unit =
    fail(s"Dereference $deref may not cause insufficient permission")
  override def preconditionFailed(failure: ContractFailure, invocation: Invocation): Unit =
    fail(s"Precondition may not fail with $failure in $invocation")
  override def postconditionFailed(failure: ContractFailure, invokable: ContractApplicable): Unit =
    fail(s"Postcondition may not fail with $failure in $invokable")
  override def divisionByZero(div: DividingExpr): Unit =
    fail(s"Divisor may not be zero in $div")
  override def exhaleFailed(failure: ContractFailure, exhale: Exhale): Unit =
    fail(s"Exhale may not fail with $failure in $exhale")
  override def internalError(description: String): Unit =
    fail(s"Internal error: $description")
  override def silverUnfoldFailed(failure: ContractFailure, unfold: SilverUnfold): Unit =
    fail(s"Unfold may not fail with $failure in $unfold")
  override def assertFailed(failure: ContractFailure, assertion: Assert): Unit =
    fail(s"Assert may not fail with $failure in $assertion")
  override def silverFoldFailed(failure: ContractFailure, fold: SilverFold): Unit =
    fail(s"Fold may not fail with $failure in $fold")
  override def silverWhileInvariantNotEstablished(failure: ContractFailure, loop: SilverWhile): Unit =
    fail(s"Loop invariant was not established due to $failure in $loop")
  override def silverWhileInvariantNotMaintained(failure: ContractFailure, loop: SilverWhile): Unit =
    fail(s"Loop invariant was not maintained due to $failure in $loop")
  override def seqBoundNegative(subscript: SeqSubscript): Unit =
    fail(s"Index of sequence subscript may be negative in $subscript")
  override def seqBoundExceedsLength(subscript: SeqSubscript): Unit =
    fail(s"Index of sequence subscript may exceed length in $subscript")
  override def labelNotReached(old: Old): Unit =
    fail(s"Label may have not been reached in evaluation of expression in old state in $old")
}

class ExpectedErrorsRegistry {
  val errors: mutable.Set[ExpectedError] = mutable.Set()

  def check(): Unit =
    errors.foreach(e => assert(e.tripped, e.whenNotTripped))
}

abstract class ExpectedError(implicit val registry: ExpectedErrorsRegistry) extends NoErrors {
  var tripped = false

  registry.errors += this

  def whenNotTripped: String
  def trip(): Unit = tripped = true
}

case class ExpectSilverAssignFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected assignment to fail"
  override def silverAssignFailed(assign: SilverFieldAssign): Unit = trip()
}

case class ExpectSilverInsufficientPermission()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected there to be insufficient permission"
  override def silverInsufficientPermission(deref: SilverDeref): Unit = trip()
}

case class ExpectPreconditionFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected precondition to fail"
  override def preconditionFailed(failure: ContractFailure, invocation: Invocation): Unit = trip()
}

case class ExpectPostconditionFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected postcondition to fail"
  override def postconditionFailed(failure: ContractFailure, invokable: ContractApplicable): Unit = trip()
}

case class ExpectDivisionByZero()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected division by zero error"
  override def divisionByZero(div: DividingExpr): Unit = trip()
}

case class ExpectExhaleFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected exhale to fail"
  override def exhaleFailed(failure: ContractFailure, exhale: Exhale): Unit = trip()
}

case class ExpectInternalError()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected internal error"
  override def internalError(description: String): Unit = trip()
}

case class ExpectSilverUnfoldFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected unfold to fail"
  override def silverUnfoldFailed(failure: ContractFailure, unfold: SilverUnfold): Unit = trip()
}

case class ExpectAssertFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected assertion to fail"
  override def assertFailed(failure: ContractFailure, assertion: Assert): Unit = trip()
}

case class ExpectSilverFoldFailed()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected fold to fail"
  override def silverFoldFailed(failure: ContractFailure, fold: SilverFold): Unit = trip()
}

case class ExpectSilverWhileInvariantNotEstablished()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected invariant not to be established"
  override def silverWhileInvariantNotEstablished(failure: ContractFailure, loop: SilverWhile): Unit = trip()
}

case class ExpectSilverWhileInvariantNotMaintained()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected invariant not to be maintained"
  override def silverWhileInvariantNotMaintained(failure: ContractFailure, loop: SilverWhile): Unit = trip()
}

case class ExpectSeqBoundNegative()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected index of sequence subscript to be negative"
  override def seqBoundNegative(subscript: SeqSubscript): Unit = trip()
}

case class ExpectSeqBoundExceedsLength()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected index of sequence subscript to exceed length"
  override def seqBoundExceedsLength(subscript: SeqSubscript): Unit = trip()
}

case class ExpectLabelNotReached()(implicit registry: ExpectedErrorsRegistry) extends ExpectedError {
  override def whenNotTripped: String = s"Expected label not to be reached in evaluation of expression in old state"
  override def labelNotReached(old: Old): Unit = trip()
}
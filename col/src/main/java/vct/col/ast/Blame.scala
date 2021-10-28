package vct.col.ast

import vct.result.VerificationResult
import vct.col.util.ExpectedError

sealed trait ContractFailure
case class ContractFalse(node: Expr) extends ContractFailure {
  override def toString: String = s"it may be false"
}
case class InsufficientPermissionToExhale(node: SilverResource) extends ContractFailure {
  override def toString: String = s"there might not be enough permission to exhale this amount"
}
case class ReceiverNotInjective(node: SilverResource) extends ContractFailure {
  override def toString: String = s"the location in this permission predicate may not be injective with regards to the quantified variables"
}
case class NegativePermissionValue(node: SilverResource) extends ContractFailure {
  override def toString: String = s"the amount of permission in this permission predicate may be negative"
}

trait VerificationFailure {
  def code: String
}

case class ExpectedErrorTrippedTwice(err: ExpectedError) extends VerificationFailure {
  override def toString: String = s"The expected error with code ${err.errorCode} occurred multiple times."
  override def code: String = "trippedTwice"
}

case class ExpectedErrorNotTripped(err: ExpectedError) extends VerificationFailure {
  override def toString: String = s"The expected error with code ${err.errorCode} was not encountered."
  override def code: String = "notTripped"
}

case class InternalError(description: String) extends VerificationFailure {
  override def toString: String = s"An internal error occurred: $description."
  override def code: String = "internal"
}
case class SilverAssignFailed(assign: SilverFieldAssign) extends VerificationFailure {
  override def toString: String = s"Insufficient permission to assign to field."
  override def code: String = "failed"
}
case class AssertFailed(failure: ContractFailure, assertion: Assert) extends VerificationFailure {
  override def toString: String = s"Assertion may not hold, since $failure."
  override def code: String = "failed"
}
case class ExhaleFailed(failure: ContractFailure, exhale: Exhale) extends VerificationFailure {
  override def toString: String = s"Exhale may fail, since $failure."
  override def code: String = "failed"
}
case class SilverUnfoldFailed(failure: ContractFailure, unfold: SilverUnfold) extends VerificationFailure {
  override def toString: String = s"Unfold may fail, since $failure."
  override def code: String = "failed"
}
case class SilverFoldFailed(failure: ContractFailure, fold: SilverFold) extends VerificationFailure {
  override def toString: String = s"Fold may fail, since $failure"
  override def code: String = "failed"
}
case class PreconditionFailed(failure: ContractFailure, invocation: Invocation) extends VerificationFailure {
  override def toString: String = s"Precondition may not hold, since $failure."
  override def code: String = "preFailed"
}
case class PostconditionFailed(failure: ContractFailure, invokable: ContractApplicable) extends VerificationFailure {
  override def toString: String = s"Postcondition may not hold, since $failure."
  override def code: String = "postFailed"
}
sealed trait SilverWhileInvariantFailure extends VerificationFailure
case class SilverWhileInvariantNotEstablished(failure: ContractFailure, loop: SilverWhile) extends SilverWhileInvariantFailure {
  override def toString: String = s"This invariant may not be established, since $failure."
  override def code: String = "notEstablished"
}
case class SilverWhileInvariantNotMaintained(failure: ContractFailure, loop: SilverWhile) extends SilverWhileInvariantFailure {
  override def toString: String = s"This invariant may not be maintained, since $failure."
  override def code: String = "notMaintained"
}
case class DivByZero(div: DividingExpr) extends VerificationFailure {
  override def toString: String = s"The divisor may be zero."
  override def code: String = "divByZero"
}
sealed trait DerefInsufficientPermission extends VerificationFailure
case class InsufficientPermission(deref: HeapDeref) extends DerefInsufficientPermission {
  override def toString: String = s"There may be insufficient permission to access this field here."
  override def code: String = "perm"
}
case class ModelInsufficientPermission(deref: ModelDeref) extends DerefInsufficientPermission {
  override def toString: String = s"There may be insufficient permission to access this model field here."
  override def code: String = "modelPerm"
}
case class LabelNotReached(old: Old) extends VerificationFailure {
  override def toString: String = s"The label mentioned in this old expression may not be reached at the time the old expression is reached."
  override def code: String = "notReached"
}
sealed trait SeqBoundFailure extends VerificationFailure
case class SeqBoundNegative(subscript: SeqSubscript) extends SeqBoundFailure {
  override def toString: String = s"The index in this sequence subscript may be negative."
  override def code: String = "indexNegative"
}
case class SeqBoundExceedsLength(subscript: SeqSubscript) extends SeqBoundFailure {
  override def toString: String = s"The index in this sequence subscript may exceed the length of the sequence."
  override def code: String = "indexExceedsLength"
}

case class ParInvariantNotEstablished(failure: ContractFailure, invariant: ParInvariant) extends VerificationFailure {
  override def toString: String = s"This parallel invariant may not be established, since $failure."
  override def code: String = "notEstablished"
}
sealed trait ParBarrierFailed extends VerificationFailure
case class ParBarrierNotEstablished(failure: ContractFailure, barrier: ParBarrier) extends ParBarrierFailed {
  override def toString: String = s"The precondition of this barrier may not hold, since $failure."
  override def code: String = "notEstablished"
}
case class ParBarrierInconsistent(failure: ContractFailure, barrier: ParBarrier) extends ParBarrierFailed {
  override def toString: String = s"The precondition of this barrier is not consistent with the postcondition, since this postcondition may not hold, because $failure."
  override def code: String = "inconsistent"
}
sealed trait ParRegionFailed extends VerificationFailure
case class ParRegionPreconditionFailed(failure: ContractFailure, region: ParRegion) extends ParRegionFailed {
  override def toString: String = s"The precondition of this region may not hold, since $failure."
  override def code: String = "preFailed"
}
sealed trait ParRegionInconsistent extends ParRegionFailed {
  def failure: ContractFailure
  override def toString: String = s"The contract of the parallel region is inconsistent with the joint contracts of its blocks: $direction, since $failure."
  def direction: String
}
case class ParRegionPreconditionDoesNotImplyBlockPreconditions(failure: ContractFailure, region: ParRegion) extends ParRegionInconsistent {
  override def direction: String = s"the precondition of the region does not imply the preconditions of its blocks"
  override def code: String = "regionPreBlockPre"
}
case class ParRegionPostconditionNotImpliedByBlockPostconditions(failure: ContractFailure, region: ParRegion) extends ParRegionInconsistent {
  override def direction: String = s"the postcondition of the region does not follow from the postconditions of its blocks"
  override def code: String = "blockPostRegionPost"
}
case class OptionNone(access: OptGet) extends VerificationFailure {
  override def code: String = "optNone"
  override def toString: String = "Option may be empty."
}
case class MapKeyError(access: MapGet) extends VerificationFailure {
  override def code: String = "mapKey"
  override def toString: String = "Map may not contain this key."
}
sealed trait ArraySubscriptError extends VerificationFailure
case class ArrayNull(arr: Expr) extends ArraySubscriptError {
  override def code: String = "arrayNull"
  override def toString: String = "Array may be null."
}
case class ArrayBounds(idx: Expr) extends ArraySubscriptError {
  override def code: String = "arrayBounds"
  override def toString: String = "Index may be negative, or exceed the length of the array."
}
case class ArrayInsufficientPermission(arr: Expr) extends ArraySubscriptError {
  override def code: String = "arrayPerm"
  override def toString: String = "There may be insufficient permission to access the array."
}
sealed trait PointerSubscriptError extends VerificationFailure
sealed trait PointerDerefError extends PointerSubscriptError
case class PointerNull(pointer: Expr) extends PointerDerefError {
  override def code: String = "ptrNull"
  override def toString: String = "Pointer may be null."
}
case class PointerBounds(pointer: Expr) extends PointerSubscriptError {
  override def code: String = "ptrBlock"
  override def toString: String = "The offset to the pointer may be outside the bounds of the allocated memory area that the pointer is in."
}
case class PointerInsufficientPermission(pointer: Expr) extends PointerDerefError {
  override def code: String = "ptrPerm"
  override def toString: String = "There may be insufficient permission to dereference the pointer."
}

trait Blame[-T <: VerificationFailure] {
  def blame(error: T): Unit
}

case class BlameUnreachable(message: String, failure: VerificationFailure) extends VerificationResult.SystemError {
  def text: String = s"An error condition was reached, which should be statically unreachable. $message ($failure)"
}

case class PanicBlame(message: String) extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit = throw BlameUnreachable(message, error)
}

object DerefAssignTarget extends PanicBlame("Assigning to a field should trigger an error on the assignment, and not on the dereference.")
object DerefPerm extends PanicBlame("Dereferencing a field in a permission should trigger an error on the permission, not on the dereference.")
object UnresolvedDesignProblem extends PanicBlame("The design does not yet accommodate passing a meaningful blame here")

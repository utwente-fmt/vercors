package vct.col.origin

import vct.result.VerificationResult
import vct.col.util.ExpectedError
import vct.col.ast._

sealed trait ContractFailure
case class ContractFalse(node: Expr[_]) extends ContractFailure {
  override def toString: String = s"it may be false"
}
case class InsufficientPermissionToExhale(node: Expr[_]) extends ContractFailure {
  override def toString: String = s"there might not be enough permission to exhale this amount"
}
case class ReceiverNotInjective(node: Expr[_]) extends ContractFailure {
  override def toString: String = s"the location in this permission predicate may not be injective with regards to the quantified variables"
}
case class NegativePermissionValue(node: Expr[_]) extends ContractFailure {
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
case class AssignFailed(assign: SilverFieldAssign[_]) extends VerificationFailure {
  override def toString: String = s"Insufficient permission to assign to field."
  override def code: String = "failed"
}
case class AssertFailed(failure: ContractFailure, assertion: Assert[_]) extends VerificationFailure {
  override def toString: String = s"Assertion may not hold, since $failure."
  override def code: String = "failed"
}
case class ExhaleFailed(failure: ContractFailure, exhale: Exhale[_]) extends VerificationFailure {
  override def toString: String = s"Exhale may fail, since $failure."
  override def code: String = "failed"
}
case class UnfoldFailed(failure: ContractFailure, unfold: Unfold[_]) extends VerificationFailure {
  override def toString: String = s"Unfold may fail, since $failure."
  override def code: String = "failed"
}
case class FoldFailed(failure: ContractFailure, fold: Fold[_]) extends VerificationFailure {
  override def toString: String = s"Fold may fail, since $failure"
  override def code: String = "failed"
}
sealed trait FrontendInvocationError extends VerificationFailure
case class PreconditionFailed(failure: ContractFailure, invocation: Invocation[_]) extends FrontendInvocationError {
  override def toString: String = s"Precondition may not hold, since $failure."
  override def code: String = "preFailed"
}
case class PostconditionFailed(failure: ContractFailure, invokable: ContractApplicable[_]) extends ConstructorFailure {
  override def toString: String = s"Postcondition may not hold, since $failure."
  override def code: String = "postFailed"
}
sealed trait LoopInvariantFailure extends VerificationFailure
case class LoopInvariantNotEstablished(failure: ContractFailure, loop: Loop[_]) extends LoopInvariantFailure {
  override def toString: String = s"This invariant may not be established, since $failure."
  override def code: String = "notEstablished"
}
case class LoopInvariantNotMaintained(failure: ContractFailure, loop: Loop[_]) extends LoopInvariantFailure {
  override def toString: String = s"This invariant may not be maintained, since $failure."
  override def code: String = "notMaintained"
}
case class DivByZero(div: DividingExpr[_]) extends VerificationFailure {
  override def toString: String = s"The divisor may be zero."
  override def code: String = "divByZero"
}
sealed trait FrontendDerefError extends VerificationFailure
sealed trait FrontendPlusError extends VerificationFailure
sealed trait FrontendSubscriptError extends VerificationFailure

sealed trait DerefInsufficientPermission extends FrontendDerefError
case class InsufficientPermission(deref: HeapDeref[_]) extends DerefInsufficientPermission {
  override def toString: String = s"There may be insufficient permission to access this field here."
  override def code: String = "perm"
}
case class ModelInsufficientPermission(deref: ModelDeref[_]) extends DerefInsufficientPermission {
  override def toString: String = s"There may be insufficient permission to access this model field here."
  override def code: String = "modelPerm"
}
case class LabelNotReached(old: Old[_]) extends VerificationFailure {
  override def toString: String = s"The label mentioned in this old expression may not be reached at the time the old expression is reached."
  override def code: String = "notReached"
}
sealed trait SeqBoundFailure extends FrontendSubscriptError with BuiltinError
case class SeqBoundNegative(subscript: SeqSubscript[_]) extends SeqBoundFailure {
  override def toString: String = s"The index in this sequence subscript may be negative."
  override def code: String = "indexNegative"
}
case class SeqBoundExceedsLength(subscript: SeqSubscript[_]) extends SeqBoundFailure {
  override def toString: String = s"The index in this sequence subscript may exceed the length of the sequence."
  override def code: String = "indexExceedsLength"
}

case class ParInvariantNotEstablished(failure: ContractFailure, invariant: ParInvariant[_]) extends VerificationFailure {
  override def toString: String = s"This parallel invariant may not be established, since $failure."
  override def code: String = "notEstablished"
}
sealed trait ParBarrierFailed extends VerificationFailure
case class ParBarrierNotEstablished(failure: ContractFailure, barrier: ParBarrier[_]) extends ParBarrierFailed {
  override def toString: String = s"The precondition of this barrier may not hold, since $failure."
  override def code: String = "notEstablished"
}
case class ParBarrierInconsistent(failure: ContractFailure, barrier: ParBarrier[_]) extends ParBarrierFailed {
  override def toString: String = s"The precondition of this barrier is not consistent with the postcondition, since this postcondition may not hold, because $failure."
  override def code: String = "inconsistent"
}

sealed trait ParBlockFailure extends VerificationFailure
case class ParPreconditionFailed(failure: ContractFailure, region: ParRegion[_]) extends ParBlockFailure {
  override def toString: String = s"The precondition of this parallel region may not hold, since $failure."
  override def code: String = "parPreFailed"
}
case class ParBlockPostconditionFailed(failure: ContractFailure, block: ParBlock[_]) extends ParBlockFailure {
  override def toString: String = s"The postcondition of this parallel block may not hold, since $failure."
  override def code: String = "parPostFailed"
}

sealed trait BuiltinError extends FrontendDerefError with FrontendInvocationError
case class OptionNone(access: OptGet[_]) extends BuiltinError {
  override def code: String = "optNone"
  override def toString: String = "Option may be empty."
}
case class NotRight(access: GetRight[_]) extends BuiltinError {
  override def code: String = "left"
  override def toString: String = "Either may be left."
}
case class NotLeft(access: GetLeft[_]) extends BuiltinError {
  override def code: String = "right"
  override def toString: String = "Either may be right."
}
case class MapKeyError(access: MapGet[_]) extends BuiltinError with FrontendSubscriptError {
  override def code: String = "mapKey"
  override def toString: String = "Map may not contain this key."
}
sealed trait ArraySubscriptError extends FrontendSubscriptError
case class ArrayNull(arr: Expr[_]) extends ArraySubscriptError with BuiltinError {
  override def code: String = "arrayNull"
  override def toString: String = "Array may be null."
}
case class ArrayBounds(idx: Expr[_]) extends ArraySubscriptError {
  override def code: String = "arrayBounds"
  override def toString: String = "Index may be negative, or exceed the length of the array."
}
case class ArrayInsufficientPermission(arr: Expr[_]) extends ArraySubscriptError {
  override def code: String = "arrayPerm"
  override def toString: String = "There may be insufficient permission to access the array."
}
case class ArrayValuesError(values: Values[_]) extends VerificationFailure {
  override def code: String = "arrayValues"
  override def toString: String =
    "Array values invocation may fail, since:\n" +
      "- The array may be null, or;\n" +
      "- The specified bounds may exceed the bounds of the array, or;\n" +
      "- The lower bound may exceed the upper bound, or;\n" +
      "- There may be insufficient permission to access the array at the specified range."
}
sealed trait PointerSubscriptError extends FrontendSubscriptError
sealed trait PointerDerefError extends PointerSubscriptError
sealed trait PointerAddError extends FrontendPlusError
case class PointerNull(pointer: Expr[_]) extends PointerDerefError with PointerAddError {
  override def code: String = "ptrNull"
  override def toString: String = "Pointer may be null."
}
case class PointerBounds(pointer: Expr[_]) extends PointerSubscriptError with PointerAddError {
  override def code: String = "ptrBlock"
  override def toString: String = "The offset to the pointer may be outside the bounds of the allocated memory area that the pointer is in."
}
case class PointerInsufficientPermission(pointer: Expr[_]) extends PointerDerefError {
  override def code: String = "ptrPerm"
  override def toString: String = "There may be insufficient permission to dereference the pointer."
}

sealed trait UnlockFailure extends VerificationFailure
case class UnlockInvariantFailed(unlock: Unlock[_], failure: ContractFailure) extends UnlockFailure {
  override def code: String = "invariantFailed"
  override def toString: String = s"The lock invariant may not be exhaled here, since $failure."
}
case class LockTokenNotHeld(unlock: Unlock[_], failure: ContractFailure) extends UnlockFailure {
  override def code: String = "heldFailed"
  override def toString: String = s"The token that indicates the lock is locked (`held(obj)`) may not be exhaled here, since $failure."
}

sealed trait ConstructorFailure extends VerificationFailure
case class CommitFailed(commit: Commit[_], failure: ContractFailure) extends ConstructorFailure {
  override def code: String = "commitFailed"
  override def toString: String = s"Committing the defined resources to the lock invariant may not be possible here, since $failure."
}

case class NotifyFailed(not: Notify[_], failure: ContractFailure) extends VerificationFailure {
  override def code: String = "heldFailed"
  override def toString: String = s"The token that indicated the lock is locked (`held(obj)`) may not be asserted here, since $failure."
}

case class ThrowNull(t: Throw[_]) extends VerificationFailure {
  override def code: String = "null"
  override def toString: String = "The value thrown here may be null."
}

sealed trait UnsafeCoercion extends VerificationFailure
case class CoerceRatZFracFailed(rat: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratZfrac"
  override def toString: String = "Rational may exceed the bounds of zfrac: [0, 1]"
}
case class CoerceRatFracFailed(rat: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratFrac"
  override def toString: String = "Rational may exceed the bounds of frac: (0, 1]"
}
case class CoerceZFracFracFailed(zfrac: Expr[_]) extends UnsafeCoercion {
  override def code: String = "zfracFrac"
  override def toString: String = "zfrac may be zero."
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

object NeverNone extends PanicBlame("get in `opt == none ? _ : get(opt)` should always be ok.")
object FramedSeqIndex extends PanicBlame("access in `∀i. 0 <= i < |xs| ==> ...xs[i]...` should never be out of bounds")
object FramedArrIndex extends PanicBlame("access in `∀i. 0 <= i < xs.length ==> Perm(xs[i], read) ** ...xs[i]...` should always be ok")
object FramedArrLength extends PanicBlame("length query in `arr == null ? _ : arr.length` should always be ok.")
object FramedMapGet extends PanicBlame("access in `∀k. k \\in m.keys ==> ...m[k]...` should always be ok.")
object FramedGetLeft extends PanicBlame("left in `e.isLeft ? e.left : ...` should always be ok.")
object FramedGetRight extends PanicBlame("right in `e.isLeft ? ... : e.right` should always be ok.")
object AbstractApplicable extends PanicBlame("the postcondition of an abstract applicable is not checked, and hence cannot fail.")
object TriggerPatternBlame extends PanicBlame("patterns in a trigger are not evaluated, but schematic, so any blame in a trigger is never applied.")

object AssignLocalOk extends PanicBlame("Assigning to a local can never fail.")
object DerefAssignTarget extends PanicBlame("Assigning to a field should trigger an error on the assignment, and not on the dereference.")
object SubscriptAssignTarget extends PanicBlame("Assigning to a subscript should trigger an error on the assignment, and not on the subscript.")
object DerefPerm extends PanicBlame("Dereferencing a field in a permission should trigger an error on the permission, not on the dereference.")
object ArrayPerm extends PanicBlame("Subscripting an array in a permission should trigger an error on the permission, not on the dereference.")
object UnresolvedDesignProblem extends PanicBlame("The design does not yet accommodate passing a meaningful blame here")

object JavaArrayInitializerBlame extends PanicBlame("The explicit initialization of an array in Java should never generate an assignment that exceeds the bounds of the array")
package vct.col.origin

import vct.result.VerificationResult
import vct.col.util.ExpectedError
import vct.col.ast._
import vct.result.VerificationResult.SystemError

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

sealed trait AccountedDirection
case object FailLeft extends AccountedDirection
case object FailRight extends AccountedDirection

sealed trait FrontendInvocationError extends VerificationFailure
sealed trait InvocationFailure extends FrontendInvocationError
case class PreconditionFailed(path: Seq[AccountedDirection], failure: ContractFailure, invocation: InvokingNode[_]) extends InvocationFailure {
  override def toString: String = s"Precondition may not hold, since $failure."
  override def code: String = "preFailed"
}
case class ContextEverywhereFailedInPre(failure: ContractFailure, invocation: InvokingNode[_]) extends InvocationFailure {
  override def toString: String = s"Context may not hold in precondition, since $failure."
  override def code: String = "contextPreFailed"
}

sealed trait CallableFailure extends ConstructorFailure
sealed trait ContractedFailure extends CallableFailure
case class PostconditionFailed(path: Seq[AccountedDirection], failure: ContractFailure, invokable: ContractApplicable[_]) extends ContractedFailure {
  override def toString: String = s"Postcondition may not hold, since $failure."
  override def code: String = "postFailed"
}
case class ContextEverywhereFailedInPost(failure: ContractFailure, invokable: ContractApplicable[_]) extends ContractedFailure {
  override def toString: String = s"Context may not hold in postcondition, since $failure."
  override def code: String = "contextPostFailed"
}
case class SignalsFailed(failure: ContractFailure, invokable: AbstractMethod[_]) extends CallableFailure {
  override def toString: String = s"Signals clause may not hold, since $failure."
  override def code: String = "signals"
}
case class ExceptionNotInSignals(failure: ContractFailure, invokable: AbstractMethod[_]) extends CallableFailure {
  override def toString: String = s"Method may throw exception not included in signals clauses."
  override def code: String = "extraExc"
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
case class ParInvariantNotMaintained(failure: ContractFailure, atomic: ParAtomic[_]) extends VerificationFailure {
  override def toString: String = s"The parallel invariant may not be maintained, since $failure."
  override def code: String = "notMaintained"
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
case class ParBarrierMayNotThrow(barrier: ParBarrier[_]) extends ParBarrierFailed {
  override def toString: String = "The proof hint for this barrier may throw an exception."
  override def code: String = "barrierThrows"
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
case class ParBlockMayNotThrow(failure: ContractFailure, block: ParBlock[_]) extends ParBlockFailure {
  override def toString: String = s"The implementation of this parallel block may throw an exception."
  override def code: String = "parThrows"
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

sealed trait ArrayValuesError extends VerificationFailure {
  def values: Values[_]
  override def toString: String = s"Array values invocation may fail, since $reason."
  def reason: String
}

case class ArrayValuesNull(values: Values[_]) extends ArrayValuesError {
  override def reason = "the array value may be null"
  override def code: String = "valuesNull"
}
case class ArrayValuesFromNegative(values: Values[_]) extends ArrayValuesError {
  override def reason = "the start of the range may be negative"
  override def code: String = "fromNeg"
}
case class ArrayValuesFromToOrder(values: Values[_]) extends ArrayValuesError {
  override def reason = "the start of the range may exceed the end of the range"
  override def code: String = "fromCrossesTo"
}
case class ArrayValuesToLength(values: Values[_]) extends ArrayValuesError {
  override def reason = "the end of the range may exceed the length of the array"
  override def code: String = "toLength"
}
case class ArrayValuesPerm(values: Values[_]) extends ArrayValuesError {
  override def reason = "there may be insufficient permission to access the array at the specified range"
  override def code: String = "valuesPerm"
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

case object BlamePathError extends SystemError {
  override def text: String = "The accounting for a pre- or postcondition is wrong: the path is empty before the layered blames are resolved, or an empty path was expected but it is not."
}

case object ImplBlameSplit {
  def apply(blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[CallableFailure]): ImplBlameSplit =
    new ImplBlameSplit(blames, default)

  def left(left: Blame[PostconditionFailed], right: Blame[CallableFailure]): ImplBlameSplit =
    new ImplBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right(left: Blame[CallableFailure], right: Blame[PostconditionFailed]): ImplBlameSplit =
    new ImplBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class ImplBlameSplit(blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[CallableFailure]) extends Blame[CallableFailure] {
  override def blame(error: CallableFailure): Unit = error match {
    case PostconditionFailed(path, failure, invokable) => path match {
      case Nil => throw BlamePathError
      case FailLeft :: tail => blames(FailLeft).blame(PostconditionFailed(tail, failure, invokable))
      case FailRight :: tail => blames(FailRight).blame(PostconditionFailed(tail, failure, invokable))
    }
    case context: ContextEverywhereFailedInPost => default.blame(context)
    case signals: SignalsFailed => default.blame(signals)
    case signals: ExceptionNotInSignals => default.blame(signals)
  }
}

case object ContractedBlameSplit {
  def apply(blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[ContractedFailure]): ContractedBlameSplit =
    new ContractedBlameSplit(blames, default)

  def left(left: Blame[PostconditionFailed], right: Blame[ContractedFailure]): ContractedBlameSplit =
    new ContractedBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right(left: Blame[ContractedFailure], right: Blame[PostconditionFailed]): ContractedBlameSplit =
    new ContractedBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class ContractedBlameSplit(blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[ContractedFailure]) extends Blame[ContractedFailure] {
  override def blame(error: ContractedFailure): Unit = error match {
    case PostconditionFailed(path, failure, invokable) => path match {
      case Nil => throw BlamePathError
      case FailLeft :: tail => blames(FailLeft).blame(PostconditionFailed(tail, failure, invokable))
      case FailRight :: tail => blames(FailRight).blame(PostconditionFailed(tail, failure, invokable))
    }
    case context: ContextEverywhereFailedInPost => default.blame(context)
  }
}

case object InvBlameSplit {
  def apply(blames: Map[AccountedDirection, Blame[PreconditionFailed]], default: Blame[InvocationFailure]): InvBlameSplit =
    new InvBlameSplit(blames, default)

  def left(left: Blame[PreconditionFailed], right: Blame[InvocationFailure]): InvBlameSplit =
    new InvBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right(left: Blame[InvocationFailure], right: Blame[PreconditionFailed]): InvBlameSplit =
    new InvBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}


case class InvBlameSplit(blames: Map[AccountedDirection, Blame[PreconditionFailed]], default: Blame[InvocationFailure]) extends Blame[InvocationFailure] {
  override def blame(error: InvocationFailure): Unit = error match {
    case PreconditionFailed(path, failure, invocation) => path match {
      case Nil => throw BlamePathError
      case FailLeft :: tail => blames(FailLeft).blame(PreconditionFailed(tail, failure, invocation))
      case FailRight :: tail => blames(FailRight).blame(PreconditionFailed(tail, failure, invocation))
    }
    case context: ContextEverywhereFailedInPre => default.blame(context)
  }
}

case class PreSplit(left: Blame[PreconditionFailed], right: Blame[PreconditionFailed]) extends Blame[PreconditionFailed] {
  override def blame(error: PreconditionFailed): Unit =
    error.path match {
      case Nil => throw BlamePathError
      case FailLeft :: tail => left.blame(PreconditionFailed(tail, error.failure, error.invocation))
      case FailRight :: tail => right.blame(PreconditionFailed(tail, error.failure, error.invocation))
    }
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

case class NoContext(inner: Blame[PreconditionFailed]) extends Blame[InvocationFailure] {
  override def blame(error: InvocationFailure): Unit = error match {
    case pre: PreconditionFailed => inner.blame(pre)
    case ctx: ContextEverywhereFailedInPre => PanicBlame("Function or method does not list any context_everywhere clauses, so cannot fail on a context_everywhere clause.").blame(ctx)
  }
}
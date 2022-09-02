package vct.col.origin

import vct.result.VerificationError
import vct.col.util.ExpectedError
import vct.col.ast._
import vct.result.VerificationError.SystemError

sealed trait ContractFailure {
  def node: Node[_]
}

case class ContractFalse(node: Expr[_]) extends ContractFailure {
  override def toString: String = "this expression may be false"
}
case class InsufficientPermissionToExhale(node: Expr[_]) extends ContractFailure {
  override def toString: String = "there might not be enough permission to exhale this amount"
}
case class NegativePermissionValue(node: Expr[_]) extends ContractFailure {
  override def toString: String = "the amount of permission in this permission predicate may be negative"
}

trait VerificationFailure {
  def code: String
  def text: String

  override def toString: String = text + "\n"
}

trait NodeVerificationFailure extends VerificationFailure {
  def node: Node[_]

  override def toString: String = node.o.messageInContext(text)
}

trait WithContractFailure extends NodeVerificationFailure {
  def failure: ContractFailure

  override def toString: String =
    Origin.messagesInContext(Seq(
      (node.o, text + " ..."),
      (failure.node.o, "... " + failure.toString),
    ))
}

sealed trait ExpectedErrorFailure extends VerificationFailure {
  def err: ExpectedError
}

case class ExpectedErrorTrippedTwice(err: ExpectedError, left: VerificationFailure, right: VerificationFailure) extends ExpectedErrorFailure {
  override def text: String = {
    err.errorRegion.messageInContext(s"The expected error with code `${err.errorCode}` occurred multiple times.")
  }

  override def code: String = "trippedTwice"
}

case class ExpectedErrorNotTripped(err: ExpectedError) extends ExpectedErrorFailure {
  override def text: String =
    err.errorRegion.messageInContext(s"The expected error with code `${err.errorCode}` was not encountered.")
  override def code: String = "notTripped"
}

case class AssignFailed(node: SilverFieldAssign[_]) extends NodeVerificationFailure {
  override def text: String = "Insufficient permission to assign to field."
  override def code: String = "failed"
}
case class AssertFailed(failure: ContractFailure, node: Assert[_]) extends WithContractFailure {
  override def text: String = "Assertion may not hold, since"
  override def code: String = "failed"
}
case class ExhaleFailed(failure: ContractFailure, node: Exhale[_]) extends WithContractFailure {
  override def text: String = "Exhale may fail, since"
  override def code: String = "failed"
}
case class UnfoldFailed(failure: ContractFailure, node: Unfold[_]) extends WithContractFailure {
  override def text: String = "Unfold may fail, since"
  override def code: String = "failed"
}
case class FoldFailed(failure: ContractFailure, node: Fold[_]) extends WithContractFailure {
  override def text: String = "Fold may fail, since"
  override def code: String = "failed"
}
case class SendFailed(failure: ContractFailure, node: Send[_]) extends WithContractFailure {
  override def text: String = "Send may fail, since"
  override def code: String = "failed"
}

sealed trait AccountedDirection
case object FailLeft extends AccountedDirection
case object FailRight extends AccountedDirection

sealed trait FrontendInvocationError extends NodeVerificationFailure
sealed trait InstanceInvocationFailure extends FrontendInvocationError
case class InstanceNull(node: InvokingNode[_]) extends InstanceInvocationFailure {
  override def code: String = "null"
  override def text: String = "The object for this invocation may be null."
}

sealed trait InvocationFailure extends InstanceInvocationFailure with WithContractFailure
case class PreconditionFailed(path: Seq[AccountedDirection], failure: ContractFailure, node: InvokingNode[_]) extends InvocationFailure {
  override def text: String = "Precondition may not hold, since"
  override def code: String = "preFailed"
}
case class ContextEverywhereFailedInPre(failure: ContractFailure, node: InvokingNode[_]) extends InvocationFailure {
  override def text: String = "Context may not hold in precondition, since"
  override def code: String = "contextPreFailed"
}

sealed trait CallableFailure extends ConstructorFailure
sealed trait ContractedFailure extends CallableFailure
case class PostconditionFailed(path: Seq[AccountedDirection], failure: ContractFailure, node: ContractApplicable[_]) extends ContractedFailure {
  override def text: String = "Postcondition may not hold, since"
  override def code: String = "postFailed"
}
case class ContextEverywhereFailedInPost(failure: ContractFailure, node: ContractApplicable[_]) extends ContractedFailure {
  override def text: String = "Context may not hold in postcondition, since"
  override def code: String = "contextPostFailed"
}
case class SignalsFailed(failure: ContractFailure, node: AbstractMethod[_]) extends CallableFailure {
  override def text: String = "Signals clause may not hold, since"
  override def code: String = "signals"
}
case class ExceptionNotInSignals(failure: ContractFailure, node: AbstractMethod[_]) extends CallableFailure {
  override def text: String = "Method may throw exception not included in signals clauses."
  override def code: String = "extraExc"
}
sealed trait LoopInvariantFailure extends WithContractFailure
case class LoopInvariantNotEstablished(failure: ContractFailure, node: LoopInvariant[_]) extends LoopInvariantFailure {
  override def text: String = "This invariant may not be established, since"
  override def code: String = "notEstablished"
}
case class LoopInvariantNotMaintained(failure: ContractFailure, node: LoopInvariant[_]) extends LoopInvariantFailure {
  override def text: String = "This invariant may not be maintained, since"
  override def code: String = "notMaintained"
}
case class ReceiverNotInjective(node: Starall[_]) extends NodeVerificationFailure with AnyStarError {
  override def text: String = "The location of the permission predicate in this quantifier may not be injective with regards to the quantified variables."
  override def code: String = "notInjective"
}
case class DivByZero(node: DividingExpr[_]) extends NodeVerificationFailure {
  override def text: String = "The divisor may be zero."
  override def code: String = "divByZero"
}
sealed trait FrontendDerefError extends NodeVerificationFailure
sealed trait FrontendPlusError extends NodeVerificationFailure
sealed trait FrontendSubscriptError extends NodeVerificationFailure

sealed trait DerefInsufficientPermission extends FrontendDerefError
case class InsufficientPermission(node: HeapDeref[_]) extends DerefInsufficientPermission {
  override def text: String = "There may be insufficient permission to access this field here."
  override def code: String = "perm"
}
case class ModelInsufficientPermission(node: ModelDeref[_]) extends DerefInsufficientPermission {
  override def text: String = "There may be insufficient permission to access this model field here."
  override def code: String = "modelPerm"
}
case class LabelNotReached(node: Old[_]) extends NodeVerificationFailure {
  override def text: String = "The label mentioned in this old expression may not be reached at the time the old expression is reached."
  override def code: String = "notReached"
}
sealed trait SeqBoundFailure extends FrontendSubscriptError with BuiltinError
case class SeqBoundNegative(node: SeqSubscript[_]) extends SeqBoundFailure {
  override def text: String = "The index in this sequence subscript may be negative."
  override def code: String = "indexNegative"
}
case class SeqBoundExceedsLength(node: SeqSubscript[_]) extends SeqBoundFailure {
  override def text: String = "The index in this sequence subscript may exceed the length of the sequence."
  override def code: String = "indexExceedsLength"
}

case class ParInvariantNotEstablished(failure: ContractFailure, node: ParInvariant[_]) extends WithContractFailure {
  override def text: String = "This parallel invariant may not be established, since"
  override def code: String = "notEstablished"
}
case class ParInvariantNotMaintained(failure: ContractFailure, node: ParAtomic[_]) extends WithContractFailure {
  override def text: String = "The parallel invariant may not be maintained, since"
  override def code: String = "notMaintained"
}
sealed trait ParBarrierFailed extends NodeVerificationFailure
case class ParBarrierNotEstablished(failure: ContractFailure, node: ParBarrier[_]) extends ParBarrierFailed with WithContractFailure {
  override def text: String = "The precondition of this barrier may not hold, since"
  override def code: String = "notEstablished"
}
case class ParBarrierInconsistent(failure: ContractFailure, node: ParBarrier[_]) extends ParBarrierFailed with WithContractFailure {
  override def text: String = "The precondition of this barrier is not consistent with the postcondition, since this postcondition may not hold, because"
  override def code: String = "inconsistent"
}
case class ParBarrierMayNotThrow(node: ParBarrier[_]) extends ParBarrierFailed {
  override def text: String = "The proof hint for this barrier may throw an exception."
  override def code: String = "barrierThrows"
}

sealed trait ParBlockFailure extends VerificationFailure
case class ParPredicateNotInjective(block: ParBlock[_], predicate: Expr[_]) extends ParBlockFailure {
  override def code: String = "parNotInjective"
  override def text: String =
    Origin.messagesInContext(Seq(
      (block.o, "This parallel block causes the formulas in its body to be quantified over all threads, ..."),
      (predicate.o, "... but this expression could not be simplified, and the Perm location is not injective in the thread variables."),
    ))
}

sealed trait ParBlockContractFailure extends ParBlockFailure with WithContractFailure
case class ParPreconditionFailed(failure: ContractFailure, node: ParRegion[_]) extends ParBlockContractFailure {
  override def text: String = "The precondition of this parallel region may not hold, since"
  override def code: String = "parPreFailed"
}
case class ParBlockPostconditionFailed(failure: ContractFailure, node: ParBlock[_]) extends ParBlockContractFailure {
  override def text: String = "The postcondition of this parallel block may not hold, since"
  override def code: String = "parPostFailed"
}
case class ParBlockMayNotThrow(failure: ContractFailure, node: ParBlock[_]) extends ParBlockContractFailure {
  override def text: String = "The implementation of this parallel block may throw an exception."
  override def code: String = "parThrows"
}

sealed trait BuiltinError extends FrontendDerefError with FrontendInvocationError
case class OptionNone(node: OptGet[_]) extends BuiltinError {
  override def code: String = "optNone"
  override def text: String = "Option may be empty."
}
case class NotRight(node: GetRight[_]) extends BuiltinError {
  override def code: String = "left"
  override def text: String = "Either may be left."
}
case class NotLeft(node: GetLeft[_]) extends BuiltinError {
  override def code: String = "right"
  override def text: String = "Either may be right."
}
case class MapKeyError(node: MapGet[_]) extends BuiltinError with FrontendSubscriptError {
  override def code: String = "mapKey"
  override def text: String = "Map may not contain this key."
}
sealed trait ArraySubscriptError extends FrontendSubscriptError
sealed trait AnyStarError extends VerificationFailure
case class ArrayNull(node: Expr[_]) extends ArraySubscriptError with BuiltinError with AnyStarError {
  override def code: String = "arrayNull"
  override def text: String = "Array may be null."
}
case class ArrayBounds(node: Expr[_]) extends ArraySubscriptError {
  override def code: String = "arrayBounds"
  override def text: String = "Index may be negative, or exceed the length of the array."
}
case class ArrayInsufficientPermission(node: Expr[_]) extends ArraySubscriptError {
  override def code: String = "arrayPerm"
  override def text: String = "There may be insufficient permission to access the array."
}

sealed trait ArrayValuesError extends NodeVerificationFailure {
  def node: Values[_]
  override def text: String = s"Array values invocation may fail, since $reason."
  def reason: String
}

case class ArrayValuesNull(node: Values[_]) extends ArrayValuesError {
  override def reason = "the array value may be null"
  override def code: String = "nodeNull"
}
case class ArrayValuesFromNegative(node: Values[_]) extends ArrayValuesError {
  override def reason = "the start of the range may be negative"
  override def code: String = "fromNeg"
}
case class ArrayValuesFromToOrder(node: Values[_]) extends ArrayValuesError {
  override def reason = "the start of the range may exceed the end of the range"
  override def code: String = "fromCrossesTo"
}
case class ArrayValuesToLength(node: Values[_]) extends ArrayValuesError {
  override def reason = "the end of the range may exceed the length of the array"
  override def code: String = "toLength"
}
case class ArrayValuesPerm(node: Values[_]) extends ArrayValuesError {
  override def reason = "there may be insufficient permission to access the array at the specified range"
  override def code: String = "valuesPerm"
}

sealed trait PointerSubscriptError extends FrontendSubscriptError
sealed trait PointerDerefError extends PointerSubscriptError
sealed trait PointerAddError extends FrontendPlusError
case class PointerNull(node: Expr[_]) extends PointerDerefError with PointerAddError {
  override def code: String = "ptrNull"
  override def text: String = "Pointer may be null."
}
case class PointerBounds(node: Expr[_]) extends PointerSubscriptError with PointerAddError {
  override def code: String = "ptrBlock"
  override def text: String = "The offset to the pointer may be outside the bounds of the allocated memory area that the pointer is in."
}
case class PointerInsufficientPermission(node: Expr[_]) extends PointerDerefError {
  override def code: String = "ptrPerm"
  override def text: String = "There may be insufficient permission to dereference the pointer."
}

sealed trait UnlockFailure extends WithContractFailure
case class UnlockInvariantFailed(node: Unlock[_], failure: ContractFailure) extends UnlockFailure {
  override def code: String = "invariantFailed"
  override def text: String = "The lock invariant may not be exhaled here, since"
}
case class LockTokenNotHeld(node: Unlock[_], failure: ContractFailure) extends UnlockFailure {
  override def code: String = "heldFailed"
  override def text: String = "The token that indicates the lock is locked (`held(obj)`) may not be exhaled here, since"
}

sealed trait ConstructorFailure extends WithContractFailure
case class CommitFailed(node: Commit[_], failure: ContractFailure) extends ConstructorFailure {
  override def code: String = "commitFailed"
  override def text: String = "Committing the defined resources to the lock invariant may not be possible here, since"
}

case class NotifyFailed(node: Notify[_], failure: ContractFailure) extends WithContractFailure {
  override def code: String = "heldFailed"
  override def text: String = "The token that indicated the lock is locked (`held(obj)`) may not be asserted here, since"
}

case class ThrowNull(node: Throw[_]) extends NodeVerificationFailure {
  override def code: String = "null"
  override def text: String = "The value thrown here may be null."
}

case class ScaleNegative(node: Scale[_]) extends NodeVerificationFailure {
  override def code: String = "scaleNeg"
  override def text: String = "The scale value here may be negative."
}

sealed trait UnsafeCoercion extends NodeVerificationFailure
case class CoerceRatZFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratZfrac"
  override def text: String = "Rational may exceed the bounds of zfrac: [0, 1]"
}
case class CoerceRatFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratFrac"
  override def text: String = "Rational may exceed the bounds of frac: (0, 1]"
}
case class CoerceZFracFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "zfracFrac"
  override def text: String = "zfrac may be zero."
}

sealed trait JavaAnnotationFailure extends VerificationFailure

sealed trait BipTransitionFailure extends CallableFailure
case class BipComponentInvariantNotMaintained(failure: ContractFailure, node: BipTransition[_]) extends BipTransitionFailure {
  override def code: String = "bipComponentInvariantNotMaintained"
  override def text: String = "In this transition the invariant of the component is not maintained, since"
}
case class BipStateInvariantNotMaintained(failure: ContractFailure, node: BipTransition[_]) extends BipTransitionFailure {
  override def code: String = "bipStateInvariantNotMaintained"
  override def text: String = "In this transition the invariant of the state is not maintained, since"
}
case class BipTransitionPostconditionFailure(failure: ContractFailure, node: BipTransition[_]) extends BipTransitionFailure {
  override def code: String = "bipTransitionPostconditionFailure"
  override def text: String = "The postcondition of the transition is not maintained, since"
}
case class BipGuardInvocationFailure(node: BipTransition[_], failure: ContractFailure) extends ContractFailure with BipTransitionFailure {
  override def code: String = "bipTransitionGuardInvocation"
  override def text: String = "Invocation of transition guard in precondition of transition failed, since"
}

trait Blame[-T <: VerificationFailure] {
  def blame(error: T): Unit
}

case class FilterExpectedErrorBlame(otherwise: Blame[VerificationFailure], expectedError: ExpectedError) extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit =
    if(error.code == expectedError.errorCode) {
      expectedError.trip(error)
    } else {
      otherwise.blame(error)
    }
}

case object BlamePathError extends SystemError {
  override def text: String = "The accounting for a pre- or postcondition is wrong: the path is empty before the layered blames are resolved, or an empty path was expected but it is not."
}

case object PostBlameSplit {
  def apply[T >: PostconditionFailed <: VerificationFailure](blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[T]): PostBlameSplit[T] =
    new PostBlameSplit(blames, default)

  def left[T >: PostconditionFailed <: VerificationFailure](left: Blame[PostconditionFailed], right: Blame[T]): PostBlameSplit[T] =
    new PostBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right[T >: PostconditionFailed <: VerificationFailure](left: Blame[T], right: Blame[PostconditionFailed]): PostBlameSplit[T] =
    new PostBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class PostBlameSplit[T >: PostconditionFailed <: VerificationFailure](blames: Map[AccountedDirection, Blame[PostconditionFailed]], default: Blame[T]) extends Blame[T] {
  override def blame(error: T): Unit = error match {
    case PostconditionFailed(path, failure, invokable) => path match {
      case Nil => throw BlamePathError
      case FailLeft :: tail => blames(FailLeft).blame(PostconditionFailed(tail, failure, invokable))
      case FailRight :: tail => blames(FailRight).blame(PostconditionFailed(tail, failure, invokable))
    }
    case other => default.blame(other)
  }
}

case object PreBlameSplit {
  def apply[T >: PreconditionFailed <: VerificationFailure](blames: Map[AccountedDirection, Blame[PreconditionFailed]], default: Blame[T]): PreBlameSplit[T] =
    new PreBlameSplit(blames, default)

  def left[T >: PreconditionFailed <: VerificationFailure](left: Blame[PreconditionFailed], right: Blame[T]): PreBlameSplit[T] =
    new PreBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right[T >: PreconditionFailed <: VerificationFailure](left: Blame[T], right: Blame[PreconditionFailed]): PreBlameSplit[T] =
    new PreBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class PreBlameSplit[T >: PreconditionFailed <: VerificationFailure](blames: Map[AccountedDirection, Blame[PreconditionFailed]], default: Blame[T]) extends Blame[T] {
  override def blame(error: T): Unit = error match {
    case PreconditionFailed(path, failure, invokable) => path match {
      case Nil =>
        throw BlamePathError
      case FailLeft :: tail => blames(FailLeft).blame(PreconditionFailed(tail, failure, invokable))
      case FailRight :: tail => blames(FailRight).blame(PreconditionFailed(tail, failure, invokable))
    }
    case other => default.blame(other)
  }
}

case class BlameUnreachable(message: String, failure: VerificationFailure) extends VerificationError.SystemError {
  def text: String = s"An error condition was reached, which should be statically unreachable. $message. Inner failure:\n${failure.toString.split('\n').mkString(" > ", "\n > ", "")}"
}

case class PanicBlame(message: String) extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit = throw BlameUnreachable(message, error)
}

object NeverNone extends PanicBlame("get in `opt == none ? _ : get(opt)` should always be ok.")
object FramedSeqIndex extends PanicBlame("access in `∀i. 0 <= i < |xs| ==> ...xs[i]...` should never be out of bounds")
object FramedArrIndex extends PanicBlame("access in `∀i. 0 <= i < xs.length ==> Perm(xs[i], read) ** ...xs[i]...` should always be ok")
object IteratedArrayInjective extends PanicBlame("access in `∀*i. 0 <= i < xs.length ==> Perm(xs[i], _)` should always be injective")
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
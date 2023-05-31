package vct.col.failure

import vct.col.ast._

// PB: Evicted: ExpectedErrorTrippedTwice
// Not sure how to model that?

case class ExpectedErrorNotTripped(node: Node[_]) extends PotentialBooleanFailure("notTripped")

case class AssignFailed(node: SilverFieldAssign[_]) extends PotentialBooleanFailure("assignFieldFailed")

case class AssertFailed(node: Assert[_]) extends PotentialAssertionFailure("assertFailed")
case class RefuteFailed(node: Refute[_]) extends PotentialBooleanFailure("refuteFailed")
case class ExhaleFailed(node: Exhale[_]) extends PotentialAssertionFailure("exhaleFailed")
case class UnfoldFailed(node: Node[_]) extends PotentialAssertionFailure("unfoldFailed")
case class FoldFailed(node: Fold[_]) extends PotentialAssertionFailure("foldFailed")

trait PackageFailure { this: PotentialFailure => }
case class PackageThrows(node: WandPackage[_]) extends PotentialBooleanFailure("packageThrows") with PackageFailure
case class PackageFailed(node: WandPackage[_]) extends PotentialAssertionFailure("packageFailed") with PackageFailure

case class WandApplyFailed(node: WandApply[_]) extends PotentialAssertionFailure("applyFailed")
case class SendFailed(node: Send[_]) extends PotentialAssertionFailure("sendFailed")

sealed trait FramedProofFailure { this: PotentialFailure => }
case class FramedProofPreFailed(node: FramedProof[_]) extends PotentialAssertionFailure("framePre") with FramedProofFailure
case class FramedProofPostFailed(node: FramedProof[_]) extends PotentialAssertionFailure("framePost") with FramedProofFailure

sealed trait FrontendInvocationError { this: PotentialFailure => }
sealed trait InstanceInvocationFailure extends FrontendInvocationError { this: PotentialFailure => }

case class InstanceNull(node: InvokingNode[_]) extends PotentialBooleanFailure("invocationObjectNull") with InstanceInvocationFailure

sealed trait InvocationFailure extends InstanceInvocationFailure { this: PotentialFailure => }
case class PreconditionFailed(node: InvokingNode[_]) extends PotentialAssertionFailure("preFailed") with InvocationFailure
case class ContextEverywhereFailedInPre(node: InvokingNode[_]) extends PotentialAssertionFailure("contextPreFailed") with InvocationFailure

sealed trait CallableFailure extends ConstructorFailure { this: PotentialFailure => }
sealed trait ContractedFailure extends CallableFailure { this: PotentialFailure => }
case class PostconditionFailed(node: ContractApplicable[_]) extends PotentialAssertionFailure("postFailed") with ContractedFailure
case class TerminationMeasureFailed(applicable: ContractApplicable[_], node: Invocation[_], measure: DecreasesClause[_]) extends PotentialBooleanFailure("decreasesFailed") with ContractedFailure
case class ContextEverywhereFailedInPost(node: ContractApplicable[_]) extends PotentialAssertionFailure("contextPostFailed") with ContractedFailure

case class SignalsFailed(node: AbstractMethod[_]) extends PotentialAssertionFailure("signalsFailed") with CallableFailure
case class ExceptionNotInSignals(node: AbstractMethod[_]) extends PotentialBooleanFailure("extraExc") with CallableFailure

sealed trait LoopInvariantFailure { this: PotentialFailure => }
case class LoopInvariantNotEstablished(node: LoopInvariant[_]) extends PotentialAssertionFailure("invariantNotEstablished") with LoopInvariantFailure
case class LoopInvariantNotMaintained(node: LoopInvariant[_]) extends PotentialAssertionFailure("notMaintained") with LoopInvariantFailure
case class LoopTerminationMeasureFailed(node: DecreasesClause[_]) extends PotentialBooleanFailure("loopDecreasesFailed") with LoopInvariantFailure

case class ReceiverNotInjective(quantifier: Starall[_], node: Expr[_]) extends PotentialBooleanFailure("notInjective") with AnyStarError

case class DivByZero(node: DividingExpr[_]) extends PotentialBooleanFailure("divByZero")

sealed trait FrontendDerefError { this: PotentialFailure => }
sealed trait FrontendAdditiveError { this: PotentialFailure => }
sealed trait FrontendSubscriptError { this: PotentialFailure => }

case class PlusProviderNull(node: Node[_]) extends PotentialBooleanFailure("plusProviderNull") with FrontendAdditiveError

/*
case class PlusProviderInvocationFailed(innerFailure: WithContractFailure) extends FrontendAdditiveError with WithContractFailure {
  override def failure: ContractFailure = innerFailure.failure
  override def code: String = failure.code
  override def node: Node[_] = innerFailure.node
  override def baseCode: String = innerFailure.baseCode
  override def descInContext: String = innerFailure.descInContext
  override def inlineDescWithSource(node: String, failure: String): String = innerFailure.inlineDescWithSource(node, failure)
}
*/

sealed trait DerefInsufficientPermission extends FrontendDerefError { this: PotentialFailure => }
case class InsufficientPermission(node: HeapDeref[_]) extends PotentialBooleanFailure("perm") with DerefInsufficientPermission
case class ModelInsufficientPermission(node: ModelDeref[_]) extends PotentialBooleanFailure("modelPerm") with DerefInsufficientPermission

case class LabelNotReached(node: Old[_]) extends PotentialBooleanFailure("notReached")

sealed trait SeqBoundFailure extends FrontendSubscriptError with BuiltinError { this: PotentialFailure => }
case class SeqBoundNegative(node: SeqSubscript[_]) extends PotentialBooleanFailure("indexNegative") with SeqBoundFailure
case class SeqBoundExceedsLength(node: SeqSubscript[_]) extends PotentialBooleanFailure("indexExceedsLength") with SeqBoundFailure

sealed trait ForkFailure { this: PotentialFailure => }
case class ForkNull(node: Fork[_]) extends PotentialBooleanFailure("forkNull") with ForkFailure
case class RunnableNotIdle(node: Fork[_]) extends PotentialBooleanFailure("running") with ForkFailure
case class RunnablePreconditionNotEstablished(node: Fork[_]) extends PotentialAssertionFailure("forkPre") with ForkFailure

sealed trait JoinFailure { this: PotentialFailure => }
case class JoinNull(node: Join[_]) extends PotentialBooleanFailure("joinNull") with JoinFailure
case class RunnableNotRunning(node: Join[_]) extends PotentialBooleanFailure("idle") with JoinFailure

sealed trait KernelFailure { this: PotentialFailure => }
case class KernelPostconditionFailed(node: CGpgpuKernelSpecifier[_]) extends PotentialAssertionFailure("postFailed") with KernelFailure
case class KernelPredicateNotInjective(kernel: CGpgpuKernelSpecifier[_], node: Expr[_]) extends PotentialBooleanFailure("kernelNotInjective") with KernelFailure

sealed trait KernelBarrierFailure { this: PotentialFailure => }
case class KernelBarrierNotEstablished(node: GpgpuBarrier[_]) extends PotentialAssertionFailure("notEstablished") with KernelBarrierFailure
case class KernelBarrierInconsistent(node: GpgpuBarrier[_]) extends PotentialAssertionFailure("inconsistent") with KernelBarrierFailure
case class KernelBarrierInvariantBroken(node: GpgpuBarrier[_]) extends PotentialAssertionFailure("barrierInvariant") with KernelBarrierFailure

case class ParInvariantNotEstablished(node: ParInvariant[_]) extends PotentialAssertionFailure("notEstablished")
case class ParInvariantNotMaintained(node: ParAtomic[_]) extends PotentialAssertionFailure("notMaintained")

sealed trait ParBarrierFailure { this: PotentialFailure => }
case class ParBarrierNotEstablished(node: ParBarrier[_]) extends PotentialAssertionFailure("notEstablished") with ParBarrierFailure
case class ParBarrierInconsistent(node: ParBarrier[_]) extends PotentialAssertionFailure("inconsistent") with ParBarrierFailure
case class ParBarrierMayNotThrow(node: ParBarrier[_]) extends PotentialBooleanFailure("barrierThrows") with ParBarrierFailure
case class ParBarrierInvariantBroken(node: ParBarrier[_]) extends PotentialAssertionFailure("barrierInvariant") with ParBarrierFailure

sealed trait ParBlockFailure { this: PotentialFailure => }
case class ParPredicateNotInjective(block: ParBlock[_], node: Expr[_]) extends PotentialBooleanFailure("parNotInjective") with ParBlockFailure

sealed trait ParBlockContractFailure extends ParBlockFailure { this: PotentialFailure => }
case class ParPreconditionFailed(node: ParRegion[_]) extends PotentialAssertionFailure("parPreFailed") with ParBlockContractFailure
case class ParBlockPostconditionFailed(node: ParBlock[_]) extends PotentialAssertionFailure("parPostFailed") with ParBlockContractFailure
case class ParBlockMayNotThrow(node: ParBlock[_]) extends PotentialBooleanFailure("parThrows") with ParBlockContractFailure

sealed trait BuiltinError extends FrontendDerefError with FrontendInvocationError { this: PotentialFailure => }
case class OptionNone(node: OptGet[_]) extends PotentialBooleanFailure("optNone") with BuiltinError
case class NotRight(node: GetRight[_]) extends PotentialBooleanFailure("left") with BuiltinError
case class NotLeft(node: GetLeft[_]) extends PotentialBooleanFailure("right") with BuiltinError
case class MapKeyError(node: MapGet[_]) extends PotentialBooleanFailure("mapKey") with BuiltinError with FrontendSubscriptError

sealed trait ArraySizeError { this: PotentialFailure => }
sealed trait ArraySubscriptError extends FrontendSubscriptError { this: PotentialFailure => }
sealed trait ArrayLocationError extends ArraySubscriptError { this: PotentialFailure => }
sealed trait AnyStarError { this: PotentialFailure => }
case class ArrayNull(node: Expr[_]) extends PotentialBooleanFailure("arrayNull") with ArrayLocationError with BuiltinError with AnyStarError
case class ArraySize(node: Expr[_]) extends PotentialBooleanFailure("arraySize") with ArraySizeError
case class ArrayBounds(node: Node[_]) extends PotentialBooleanFailure("arrayBounds") with ArrayLocationError
case class ArrayInsufficientPermission(node: Expr[_]) extends PotentialBooleanFailure("arrayPerm") with ArraySubscriptError

sealed trait ArrayValuesError { this: PotentialFailure =>
  def node: Values[_]
}

case class ArrayValuesNull(node: Values[_]) extends PotentialBooleanFailure("arrayNull") with ArrayValuesError
case class ArrayValuesFromNegative(node: Values[_]) extends PotentialBooleanFailure("fromNeg") with ArrayValuesError
case class ArrayValuesFromToOrder(node: Values[_]) extends PotentialBooleanFailure("fromCrossesTo") with ArrayValuesError
case class ArrayValuesToLength(node: Values[_]) extends PotentialBooleanFailure("toLength") with ArrayValuesError
case class ArrayValuesPerm(node: Values[_]) extends PotentialBooleanFailure("valuesPerm") with ArrayValuesError

sealed trait PointerSubscriptError extends FrontendSubscriptError { this: PotentialFailure => }
sealed trait PointerDerefError extends PointerSubscriptError { this: PotentialFailure => }
sealed trait PointerLocationError extends PointerDerefError { this: PotentialFailure => }
sealed trait PointerAddError extends FrontendAdditiveError { this: PotentialFailure => }
case class PointerNull(node: Expr[_]) extends PotentialBooleanFailure("ptrNull") with PointerLocationError with PointerAddError
case class PointerBounds(node: Node[_]) extends PotentialBooleanFailure("ptrBlock") with PointerSubscriptError with PointerAddError
case class PointerInsufficientPermission(node: Expr[_]) extends PotentialBooleanFailure("ptrPerm") with PointerDerefError

sealed trait LockRegionFailure { this: PotentialFailure => }

sealed trait LockFailure extends LockRegionFailure { this: PotentialFailure => }
case class LockObjectNull(node: Node[_]) extends PotentialBooleanFailure("lockNull") with LockFailure
case class LockNotCommitted(node: Lock[_]) extends PotentialBooleanFailure("lockNotCommitted") with LockFailure

sealed trait UnlockFailure extends LockRegionFailure { this: PotentialFailure => }
case class UnlockInvariantFailed(node: Unlock[_]) extends PotentialAssertionFailure("invariantFailed") with UnlockFailure
case class LockTokenNotHeld(node: Unlock[_]) extends PotentialAssertionFailure("heldFailed") with UnlockFailure

sealed trait ConstructorFailure { this: PotentialFailure => }
case class CommitFailed(node: Commit[_]) extends PotentialAssertionFailure("commitFailed") with ConstructorFailure

case class NotifyFailed(node: Notify[_]) extends PotentialAssertionFailure("heldFailed")

case class ThrowNull(node: Throw[_]) extends PotentialBooleanFailure("null")

case class ScaleNegative(node: Scale[_]) extends PotentialBooleanFailure("scaleNeg")

case class NontrivialUnsatisfiable(node: ApplicableContract[_]) extends PotentialBooleanFailure("unsatisfiable")

sealed trait UnsafeCoercion { this: PotentialFailure => }
case class CoerceRatZFracFailed(node: Expr[_]) extends PotentialBooleanFailure("ratZfrac") with UnsafeCoercion
case class CoerceRatFracFailed(node: Expr[_]) extends PotentialBooleanFailure("ratFrac") with UnsafeCoercion
case class CoerceZFracFracFailed(node: Expr[_]) extends PotentialBooleanFailure("zfracFrac") with UnsafeCoercion

sealed trait JavaAnnotationFailure { this: PotentialFailure => }

sealed trait BipConstructorFailure extends CallableFailure { this: PotentialFailure => }
sealed trait BipTransitionFailure extends CallableFailure { this: PotentialFailure => }

sealed trait BipTransitionContractFailure extends BipTransitionFailure { this: PotentialFailure =>
  def transition: BipTransition[_]

  override final def node: Node[_] = transition.signature
  def signature: BipTransitionSignature[_] = transition.signature
}

case class BipComponentInvariantNotEstablished(node: Procedure[_]) extends PotentialAssertionFailure("bipComponentInvariantNotEstablished") with BipConstructorFailure
case class BipStateInvariantNotEstablished(node: Procedure[_]) extends PotentialAssertionFailure("bipStateInvariantNotEstablished") with BipConstructorFailure
case class BipComponentInvariantNotMaintained(transition: BipTransition[_]) extends PotentialAssertionFailure("bipComponentInvariantNotMaintained") with BipTransitionContractFailure
case class BipStateInvariantNotMaintained(transition: BipTransition[_]) extends PotentialAssertionFailure("bipStateInvariantNotMaintained") with BipTransitionContractFailure
case class BipTransitionPostconditionFailure(transition: BipTransition[_]) extends PotentialAssertionFailure("bipTransitionPostconditionFailure") with BipTransitionContractFailure
case class BipTransitionPreconditionUnsatisfiable(node: BipTransition[_]) extends PotentialBooleanFailure("bipTransitionPreconditionUnsatisfiable") with BipTransitionFailure
case class BipOutgoingDataPreconditionUnsatisfiable(node: BipOutgoingData[_]) extends PotentialBooleanFailure("bipOutgoingDataPreconditionUnsatisfiable") with BipTransitionFailure

sealed trait BipGuardFailure extends CallableFailure { this: PotentialFailure => }
case class BipGuardPreconditionUnsatisfiable(node: BipGuard[_]) extends PotentialBooleanFailure("bipGuardPreconditionUnsatisfiable") with BipGuardFailure

sealed trait BipGlueFailure { this: PotentialFailure => }
sealed trait BipSynchronizationFailure extends BipGlueFailure { this: PotentialFailure => }
case class TransitionPreconditionFailed(synchronization: BipTransitionSynchronization[_], transition: BipTransition[_])
  extends PotentialAssertionFailure("bipTransitionPreconditionFailed") with BipSynchronizationFailure {
  override def node: Node[_] = transition.signature
}
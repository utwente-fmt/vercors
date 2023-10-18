package vct.col.origin

import com.typesafe.scalalogging.LazyLogging
import vct.result.VerificationError
import vct.col.ast._
import vct.result.VerificationError.SystemError

sealed trait ContractFailure {
  def code: String
  def node: Node[_]
  def descCompletion: String
  def inlineDescCompletion: String
}

case class ContractFalse(node: Expr[_]) extends ContractFailure {
  override def code: String = "false"
  override def descCompletion: String = "this expression may be false"
  override def inlineDescCompletion: String =
    s"`${node.o.inlineContext}` may be false"
}
case class InsufficientPermissionToExhale(node: Expr[_])
    extends ContractFailure {
  override def code: String = "perm"
  override def descCompletion: String =
    "there might not be enough permission to exhale this amount"
  override def inlineDescCompletion: String =
    s"there might be insufficient permission for `${node.o.inlineContext}`"
}
case class NegativePermissionValue(node: Expr[_]) extends ContractFailure {
  override def code: String = "negativePermValue"
  override def descCompletion: String =
    "the amount of permission in this permission predicate may be negative"
  override def inlineDescCompletion: String =
    s"`${node.o.inlineContext}` may be a negative permission amount"
}

trait VerificationFailure {
  def code: String
  def errUrl: String = s" (https://utwente.nl/vercors#$code)"

  def position: String

  def desc: String
  def inlineDesc: String

  def asTableEntry: TableEntry = TableEntry(position, code, inlineDesc)

  override def toString: String = desc + "\n"
}

object TableEntry {
  def render(entries: Seq[TableEntry]): String = {
    val posWidth = entries.map(_.position.length).max.max("At".length)
    val codeWidth = entries.map(_.code.length).max.max("Code".length)
    val descWidth = entries.map(_.desc.length).max.max("Description".length)

    val result = new StringBuilder

    result.append("At").append(" ".repeat(posWidth - "At".length)).append(" ")
    result.append("Code").append(" ".repeat(codeWidth - "Code".length))
      .append(" ")
    result.append("Description")
      .append(" ".repeat(descWidth - "Description".length)).append("\n")

    result.append("-" * posWidth).append(" ")
    result.append("-" * codeWidth).append(" ")
    result.append("-" * descWidth).append("\n")

    for (entry <- entries) {
      result.append(entry.position)
        .append(" " * (posWidth - entry.position.length)).append(" ")
      result.append(entry.code).append(" " * (codeWidth - entry.code.length))
        .append(" ")
      result.append(entry.desc).append("\n")
    }

    result.toString()
  }
}

case class TableEntry(position: String, code: String, desc: String)

trait NodeVerificationFailure extends VerificationFailure {
  def node: Node[_]

  def descInContext: String
  def inlineDescWithSource(source: String): String

  override def position: String = node.o.shortPosition
  override def desc: String = node.o.messageInContext(descInContext + errUrl)
  override def inlineDesc: String = inlineDescWithSource(node.o.inlineContext)
}

trait WithContractFailure extends VerificationFailure {
  def node: Node[_]
  def failure: ContractFailure

  def baseCode: String
  def descInContext: String
  def inlineDescWithSource(node: String, failure: String): String

  override def position: String = node.o.shortPosition

  override def code: String = s"$baseCode:${failure.code}"

  override def desc: String =
    Origin.messagesInContext(Seq(
      (node.o, descInContext + " ..."),
      (failure.node.o, "... " + failure.descCompletion + errUrl),
    ))

  override def inlineDesc: String =
    inlineDescWithSource(node.o.inlineContext, failure.inlineDescCompletion)
}

sealed trait ExpectedErrorFailure extends VerificationFailure {
  def err: ExpectedError
}

case class ExpectedErrorTrippedTwice(
    err: ExpectedError,
    left: VerificationFailure,
    right: VerificationFailure,
) extends ExpectedErrorFailure {
  override def code: String = "trippedTwice"
  override def position: String = err.errorRegion.shortPosition
  override def desc: String =
    err.errorRegion.messageInContext(
      s"The expected error with code `${err.errorCode}` occurred multiple times." +
        errUrl
    )
  override def inlineDesc: String =
    s"The expected error with code `${err.errorCode}` occurred multiple times."
}

case class ExpectedErrorNotTripped(err: ExpectedError)
    extends ExpectedErrorFailure {
  override def code: String = "notTripped"
  override def position: String = err.errorRegion.shortPosition
  override def desc: String =
    err.errorRegion.messageInContext(
      s"The expected error with code `${err.errorCode}` was not encountered." +
        errUrl
    )
  override def inlineDesc: String =
    s"The expected error with code `${err.errorCode}` was not encountered."
}

case class AssignFailed(node: SilverFieldAssign[_])
    extends NodeVerificationFailure {
  override def code: String = "assignFieldFailed"
  override def descInContext: String =
    "Insufficient permission to assign to field."
  override def inlineDescWithSource(source: String): String =
    s"Insufficient permission for assignment `$source`."
}
case class AssertFailed(failure: ContractFailure, node: Assert[_])
    extends WithContractFailure {
  override def baseCode: String = "assertFailed"
  override def descInContext: String = "Assertion may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Assertion `$node` may fail, since $failure."
}
case class RefuteFailed(node: Refute[_]) extends NodeVerificationFailure {
  override def code: String = "refuteFailed"
  override def descInContext: String =
    "Assertion holds in all cases where it is reachable."
  override def inlineDescWithSource(source: String): String =
    s"Assertion `$source` holds in all cases where it is reachable."
}
case class ExhaleFailed(failure: ContractFailure, node: Exhale[_])
    extends WithContractFailure {
  override def baseCode: String = "exhaleFailed"
  override def descInContext: String = "Exhale may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
case class UnfoldFailed(failure: ContractFailure, node: Node[_])
    extends WithContractFailure {
  override def baseCode: String = "unfoldFailed"
  override def descInContext: String = "Unfold may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
case class FoldFailed(failure: ContractFailure, node: Fold[_])
    extends WithContractFailure {
  override def baseCode: String = "foldFailed"
  override def descInContext: String = "Fold may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
trait PackageFailure extends VerificationFailure
case class PackageThrows(node: WandPackage[_])
    extends PackageFailure with NodeVerificationFailure {
  override def code: String = "packageThrows"
  override def descInContext: String = "Package proof may throw an exception"
  override def inlineDescWithSource(source: String): String =
    s"`$node` may throw an exception."
}
case class PackageFailed(failure: ContractFailure, node: WandPackage[_])
    extends PackageFailure with WithContractFailure {
  override def baseCode: String = "packageFailed"
  override def descInContext: String = "Package statement may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
case class WandApplyFailed(failure: ContractFailure, node: WandApply[_])
    extends WithContractFailure {
  override def baseCode: String = "applyFailed"
  override def descInContext: String = "Wand apply may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
case class SendFailed(failure: ContractFailure, node: Send[_])
    extends WithContractFailure {
  override def baseCode: String = "sendFailed"
  override def descInContext: String = "Send may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
sealed trait FramedProofFailure extends VerificationFailure
case class FramedProofPreFailed(failure: ContractFailure, node: FramedProof[_])
    extends FramedProofFailure with WithContractFailure {
  override def baseCode: String = "framePre"
  override def descInContext: String =
    "Precondition of framed statement may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Precondition of `$node` may fail, since $failure."
}
case class FramedProofPostFailed(failure: ContractFailure, node: FramedProof[_])
    extends FramedProofFailure with WithContractFailure {
  override def baseCode: String = "framePost"
  override def descInContext: String =
    "Postcondition of framed statement may fail, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Postcondition of `$node` may fail, since $failure."
}

sealed trait AccountedDirection
case object FailLeft extends AccountedDirection
case object FailRight extends AccountedDirection

sealed trait FrontendInvocationError extends VerificationFailure
sealed trait InstanceInvocationFailure extends FrontendInvocationError
case class InstanceNull(node: InvokingNode[_])
    extends InstanceInvocationFailure with NodeVerificationFailure {
  override def code: String = "invocationObjectNull"
  override def descInContext: String =
    "The object for this invocation may be null."
  override def inlineDescWithSource(source: String): String =
    s"The object in the invocation `$source` may be null"
}

sealed trait InvocationFailure extends InstanceInvocationFailure
case class PreconditionFailed(
    path: Seq[AccountedDirection],
    failure: ContractFailure,
    node: InvokingNode[_],
) extends InvocationFailure with WithContractFailure {
  override def baseCode: String = "preFailed"
  override def descInContext: String = "Precondition may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Precondition of `$node` may not hold, since $failure."
}
case class ContextEverywhereFailedInPre(
    failure: ContractFailure,
    node: InvokingNode[_],
) extends InvocationFailure with WithContractFailure {
  override def baseCode: String = "contextPreFailed"
  override def descInContext: String =
    "Context may not hold in precondition, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Context of `$node` may not hold in the precondition, since $failure."
}

sealed trait CallableFailure extends ConstructorFailure
sealed trait ContractedFailure extends CallableFailure
case class PostconditionFailed(
    path: Seq[AccountedDirection],
    failure: ContractFailure,
    node: ContractApplicable[_],
) extends ContractedFailure with WithContractFailure {
  override def baseCode: String = "postFailed"
  override def descInContext: String = "Postcondition may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Postcondition of `$node` may not hold, since $failure."
}
case class TerminationMeasureFailed(
    applicable: ContractApplicable[_],
    apply: Invocation[_],
    measure: DecreasesClause[_],
) extends ContractedFailure with VerificationFailure {
  override def code: String = "decreasesFailed"
  override def position: String = measure.o.shortPosition
  override def desc: String =
    Origin.messagesInContext(Seq(
      applicable.o -> "Applicable may not terminate, since ...",
      apply.o -> "... from this invocation ...",
      measure.o -> "... this measure may not be bounded, or may not decrease.",
    ))
  override def inlineDesc: String =
    s"`${apply.o.inlineContext}` may not terminate, since `${measure.o.inlineContext}` is not decreased or not bounded"
}
case class ContextEverywhereFailedInPost(
    failure: ContractFailure,
    node: ContractApplicable[_],
) extends ContractedFailure with WithContractFailure {
  override def baseCode: String = "contextPostFailed"
  override def descInContext: String =
    "Context may not hold in postcondition, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Context of `$node` may not hold in the postcondition, since $failure."
}
case class SignalsFailed(failure: ContractFailure, node: AbstractMethod[_])
    extends CallableFailure with WithContractFailure {
  override def baseCode: String = "signalsFailed"
  override def descInContext: String = "Signals clause may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Signals clause of `$node` may not hold, since $failure."
}
case class ExceptionNotInSignals(node: AbstractMethod[_])
    extends CallableFailure with NodeVerificationFailure {
  override def code: String = "extraExc"
  override def descInContext: String =
    "Method may throw exception not included in signals clauses."
  override def inlineDescWithSource(source: String): String =
    s"Method `$source` may throw exception not included in signals clauses."
}
sealed trait LoopInvariantFailure extends VerificationFailure
case class LoopInvariantNotEstablished(
    failure: ContractFailure,
    node: LoopInvariant[_],
) extends LoopInvariantFailure with WithContractFailure {
  override def baseCode: String = "invariantNotEstablished"
  override def descInContext: String =
    "This invariant may not be established, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Invariant of `$node` may not be established, since $failure."
}
case class LoopInvariantNotMaintained(
    failure: ContractFailure,
    node: LoopInvariant[_],
) extends LoopInvariantFailure with WithContractFailure {
  override def baseCode: String = "notMaintained"
  override def descInContext: String =
    "This invariant may not be maintained, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"Invariant of `$node` may not be maintained, since $failure."
}
case class LoopTerminationMeasureFailed(node: DecreasesClause[_])
    extends LoopInvariantFailure with NodeVerificationFailure {
  override def code: String = "loopDecreasesFailed"
  override def position: String = node.o.shortPosition
  override def descInContext: String =
    "Loop may not terminate, since this measure may not be bounded, or may not decrease."
  override def inlineDescWithSource(source: String): String =
    s"Loop may not terminate, since `${node.o.inlineContext}` may be unbounded or nondecreasing"
}
case class ReceiverNotInjective(quantifier: Starall[_], resource: Expr[_])
    extends VerificationFailure with AnyStarError {
  override def code: String = "notInjective"
  override def desc: String =
    Origin.messagesInContext(Seq(
      quantifier.o ->
        "This quantifier causes the resources in its body to be quantified, ...",
      resource.o ->
        "... but this resource may not be unique with regards to the quantified variables.",
    ))
  override def inlineDesc: String =
    s"The location of the permission predicate in `${resource.o.inlineContext}` may not be unique with regards to the quantified variables."

  override def position: String = resource.o.shortPosition
}
case class DivByZero(node: DividingExpr[_]) extends NodeVerificationFailure {
  override def code: String = "divByZero"
  override def descInContext: String = "The divisor may be zero."
  override def inlineDescWithSource(source: String): String =
    s"The divisor in `$source` may be zero."
}
sealed trait FrontendDerefError extends VerificationFailure
sealed trait FrontendAdditiveError extends VerificationFailure
sealed trait FrontendSubscriptError extends VerificationFailure

case class PlusProviderNull(node: Node[_])
    extends FrontendAdditiveError with NodeVerificationFailure {
  override def code: String = "plusProviderNull"
  override def descInContext: String =
    "This expression might be null, which is prohibited because it is the subject of a custom plus operation."
  override def inlineDescWithSource(source: String): String =
    s"The expression in $source might be null, which is prohibited for a custom plus operation."
}

case class PlusProviderInvocationFailed(innerFailure: WithContractFailure)
    extends FrontendAdditiveError with WithContractFailure {
  override def failure: ContractFailure = innerFailure.failure
  override def code: String = failure.code
  override def node: Node[_] = innerFailure.node
  override def baseCode: String = innerFailure.baseCode
  override def descInContext: String = innerFailure.descInContext
  override def inlineDescWithSource(node: String, failure: String): String =
    innerFailure.inlineDescWithSource(node, failure)
}

sealed trait DerefInsufficientPermission extends FrontendDerefError
case class InsufficientPermission(node: HeapDeref[_])
    extends DerefInsufficientPermission with NodeVerificationFailure {
  override def code: String = "perm"
  override def descInContext: String =
    "There may be insufficient permission to access this field here."
  override def inlineDescWithSource(source: String): String =
    s"There may be insufficient permission to access `$source`."
}
case class ModelInsufficientPermission(node: ModelDeref[_])
    extends DerefInsufficientPermission with NodeVerificationFailure {
  override def code: String = "modelPerm"
  override def descInContext: String =
    "There may be insufficient permission to access this model field here."
  override def inlineDescWithSource(source: String): String =
    s"There may be insufficient permission to access `$source`."
}
case class LabelNotReached(node: Old[_]) extends NodeVerificationFailure {
  override def code: String = "notReached"
  override def descInContext: String =
    "The label mentioned in this old expression may not have been reached at the time the old expression is evaluated."
  override def inlineDescWithSource(source: String): String =
    s"The label in `$source` may not have been reached at the time the old expression is evaluated."
}
sealed trait SeqBoundFailure extends FrontendSubscriptError with BuiltinError
case class SeqBoundNegative(node: SeqSubscript[_])
    extends SeqBoundFailure with NodeVerificationFailure {
  override def code: String = "indexNegative"
  override def descInContext: String =
    "The index in this sequence subscript may be negative."
  override def inlineDescWithSource(source: String): String =
    s"The index in `$source` may be negative."
}
case class SeqBoundExceedsLength(node: SeqSubscript[_])
    extends SeqBoundFailure with NodeVerificationFailure {
  override def code: String = "indexExceedsLength"
  override def descInContext: String =
    "The index in this sequence subscript may exceed the length of the sequence."
  override def inlineDescWithSource(source: String): String =
    s"The index in `$source` may exceed the length of the sequence."
}

sealed trait ForkFailure extends VerificationFailure
case class ForkNull(node: Fork[_])
    extends ForkFailure with NodeVerificationFailure {
  override def code: String = "forkNull"
  override def descInContext: String = "This runnable may be null."
  override def inlineDescWithSource(source: String): String =
    s"The runnable in `$source` may be null."
}
case class RunnableNotIdle(node: Fork[_])
    extends ForkFailure with NodeVerificationFailure {
  override def code: String = "running"
  override def descInContext: String =
    "This runnable may not be idle. (Hint: make sure the constructor ensures idle(this))"
  override def inlineDescWithSource(source: String): String =
    s"The runnable in `$source` may not be idle."
}
case class RunnablePreconditionNotEstablished(
    node: Fork[_],
    failure: ContractFailure,
) extends ForkFailure with WithContractFailure {
  override def baseCode: String = "forkPre"
  override def descInContext: String =
    "The precondition of the runnable may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of the runnable in `$node` may not hold, since $failure."
}
sealed trait JoinFailure extends VerificationFailure
case class JoinNull(node: Join[_])
    extends JoinFailure with NodeVerificationFailure {
  override def code: String = "joinNull"
  override def descInContext: String = "This runnable may be null."
  override def inlineDescWithSource(source: String): String =
    s"The runnable in `$source` may be null."
}
case class RunnableNotRunning(node: Join[_])
    extends JoinFailure with NodeVerificationFailure {
  override def code: String = "idle"
  override def descInContext: String = "This runnable may not be running."
  override def inlineDescWithSource(source: String): String =
    s"The runnable in `$source` may not be running."
}

sealed trait KernelFailure extends VerificationFailure
case class KernelPostconditionFailed(
    failure: ContractFailure,
    node: CGpgpuKernelSpecifier[_],
) extends KernelFailure with WithContractFailure {
  override def baseCode: String = "postFailed"
  override def descInContext: String =
    "The postcondition of this kernel may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The postcondition of `$node` may not hold, since $failure."
}
case class KernelPredicateNotInjective(
    kernel: CGpgpuKernelSpecifier[_],
    predicate: Expr[_],
) extends KernelFailure {
  override def code: String = "kernelNotInjective"
  override def position: String = predicate.o.shortPosition

  override def desc: String =
    Origin.messagesInContext(Seq(
      (
        kernel.o,
        "This kernel causes the formulas in its body to be quantified over all threads, ...",
      ),
      (
        predicate.o,
        "... but this expression could not be simplified, and the Perm location is not injective in the thread variables." +
          errUrl,
      ),
    ))

  override def inlineDesc: String =
    s"`${predicate.o.inlineContext}` does not have a unique location for every thread, and it could not be simplified away."
}

sealed trait KernelBarrierFailure extends VerificationFailure
case class KernelBarrierNotEstablished(
    failure: ContractFailure,
    node: GpgpuBarrier[_],
) extends KernelBarrierFailure with WithContractFailure {
  override def baseCode: String = "notEstablished"
  override def descInContext: String =
    "The precondition of this barrier may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of `$node` may not hold, since $failure."
}
case class KernelBarrierInconsistent(
    failure: ContractFailure,
    node: GpgpuBarrier[_],
) extends KernelBarrierFailure with WithContractFailure {
  override def baseCode: String = "inconsistent"
  override def descInContext: String =
    "The precondition of this barrier is not consistent with the postcondition, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of `$node` is not consistent with its postcondition, since $failure."
}
case class KernelBarrierInvariantBroken(
    failure: ContractFailure,
    node: GpgpuBarrier[_],
) extends KernelBarrierFailure with WithContractFailure {
  override def baseCode: String = "barrierInvariant"
  override def descInContext: String =
    "The barrier may not re-establish the used invariants, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may not re-established the used invariants, since $failure."
}

case class ParInvariantNotEstablished(
    failure: ContractFailure,
    node: ParInvariant[_],
) extends WithContractFailure {
  override def baseCode: String = "notEstablished"
  override def descInContext: String =
    "This parallel invariant may not be established, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may not be established, since $failure."
}
case class ParInvariantNotMaintained(
    failure: ContractFailure,
    node: ParAtomic[_],
) extends WithContractFailure {
  override def baseCode: String = "notMaintained"
  override def descInContext: String =
    "The parallel invariant may not be maintained, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may not be maintained, since $failure."
}
sealed trait ParBarrierFailure extends VerificationFailure
case class ParBarrierNotEstablished(
    failure: ContractFailure,
    node: ParBarrier[_],
) extends ParBarrierFailure with WithContractFailure {
  override def baseCode: String = "notEstablished"
  override def descInContext: String =
    "The precondition of this barrier may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of `$node` may not hold, since $failure."
}
case class ParBarrierInconsistent(failure: ContractFailure, node: ParBarrier[_])
    extends ParBarrierFailure with WithContractFailure {
  override def baseCode: String = "inconsistent"
  override def descInContext: String =
    "The precondition of this barrier is not consistent with the postcondition, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of `$node` is not consistent with its postcondition, since $failure."
}
case class ParBarrierMayNotThrow(node: ParBarrier[_])
    extends ParBarrierFailure with NodeVerificationFailure {
  override def code: String = "barrierThrows"
  override def descInContext: String =
    "The proof hint for this barrier may throw an exception."
  override def inlineDescWithSource(source: String): String =
    s"The proof hint of `$source` may throw an exception."
}
case class ParBarrierInvariantBroken(
    failure: ContractFailure,
    node: ParBarrier[_],
) extends ParBarrierFailure with WithContractFailure {
  override def baseCode: String = "barrierInvariant"
  override def descInContext: String =
    "The barrier may not re-establish the used invariants, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may not re-established the used invariants, since $failure."
}

sealed trait ParBlockFailure extends VerificationFailure
case class ParPredicateNotInjective(block: ParBlock[_], predicate: Expr[_])
    extends ParBlockFailure {
  override def code: String = "parNotInjective"
  override def position: String = predicate.o.shortPosition
  override def desc: String =
    Origin.messagesInContext(Seq(
      (
        block.o,
        "This parallel block causes the formulas in its body to be quantified over all threads, ...",
      ),
      (
        predicate.o,
        "... but this expression could not be simplified, and the Perm location is not injective in the thread variables." +
          errUrl,
      ),
    ))

  override def inlineDesc: String =
    s"`${predicate.o.inlineContext}` does not have a unique location for every thread, and it could not be simplified away."
}

sealed trait ParBlockContractFailure extends ParBlockFailure
case class ParPreconditionFailed(failure: ContractFailure, node: ParRegion[_])
    extends ParBlockContractFailure with WithContractFailure {
  override def baseCode: String = "parPreFailed"
  override def descInContext: String =
    "The precondition of this parallel region may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The precondition of `$node` may not hold, since $failure."
}
case class ParBlockPostconditionFailed(
    failure: ContractFailure,
    node: ParBlock[_],
) extends ParBlockContractFailure with WithContractFailure {
  override def baseCode: String = "parPostFailed"
  override def descInContext: String =
    "The postcondition of this parallel block may not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The postcondition of `$node` may not hold, since $failure."
}
case class ParBlockMayNotThrow(node: ParBlock[_])
    extends ParBlockContractFailure with NodeVerificationFailure {
  override def code: String = "parThrows"
  override def descInContext: String =
    s"The body of this parallel block may throw an exception."
  override def inlineDescWithSource(source: String): String =
    s"The body of `$source` may throw an exception."
}

sealed trait BuiltinError
    extends FrontendDerefError with FrontendInvocationError
case class OptionNone(node: OptGet[_])
    extends BuiltinError with NodeVerificationFailure {
  override def code: String = "optNone"
  override def descInContext: String = "Option may be empty."
  override def inlineDescWithSource(source: String): String =
    s"Option in `$source` may be empty."
}
case class NotRight(node: GetRight[_])
    extends BuiltinError with NodeVerificationFailure {
  override def code: String = "left"
  override def descInContext: String = "Either may be left."
  override def inlineDescWithSource(source: String): String =
    s"Either in `$source` may be left."
}
case class NotLeft(node: GetLeft[_])
    extends BuiltinError with NodeVerificationFailure {
  override def code: String = "right"
  override def descInContext: String = "Either may be right."
  override def inlineDescWithSource(source: String): String =
    s"Either in `$source` may be right."
}
case class MapKeyError(node: MapGet[_])
    extends BuiltinError
    with FrontendSubscriptError
    with NodeVerificationFailure {
  override def code: String = "mapKey"
  override def descInContext: String = "Map may not contain this key."
  override def inlineDescWithSource(source: String): String =
    s"Map in `$source` may not contain that key."
}
sealed trait ArraySizeError extends VerificationFailure
sealed trait ArraySubscriptError extends FrontendSubscriptError
sealed trait ArrayLocationError extends ArraySubscriptError
sealed trait AnyStarError extends VerificationFailure
case class ArrayNull(node: Expr[_])
    extends ArrayLocationError
    with BuiltinError
    with AnyStarError
    with NodeVerificationFailure {
  override def code: String = "arrayNull"
  override def descInContext: String = "Array may be null."
  override def inlineDescWithSource(source: String): String =
    s"Array `$source` may be null."
}
case class ArraySize(node: Expr[_])
    extends ArraySizeError with NodeVerificationFailure {
  override def code: String = "arraySize"
  override def descInContext: String = "Array size may be negative."
  override def inlineDescWithSource(source: String): String =
    s"Size of `$source` may be negative."
}
case class ArrayBounds(node: Node[_])
    extends ArrayLocationError with NodeVerificationFailure {
  override def code: String = "arrayBounds"
  override def descInContext: String =
    "Index may be negative, or exceed the length of the array."
  override def inlineDescWithSource(source: String): String =
    s"Index `$source` may be negative, or exceed the length of the array."
}
case class ArrayInsufficientPermission(node: Expr[_])
    extends ArraySubscriptError with NodeVerificationFailure {
  override def code: String = "arrayPerm"
  override def descInContext: String =
    "There may be insufficient permission to access the array."
  override def inlineDescWithSource(source: String): String =
    s"There may be insufficient permission to access `$source`."
}

sealed trait ArrayValuesError extends NodeVerificationFailure {
  def node: Values[_]
  override def descInContext: String =
    s"Array values invocation may fail, since $reason."
  override def inlineDescWithSource(source: String): String =
    s"`$source` may fail, since $reason."
  def reason: String
}

case class ArrayValuesNull(node: Values[_]) extends ArrayValuesError {
  override def code: String = "arrayNull"
  override def reason = "the array value may be null"
}
case class ArrayValuesFromNegative(node: Values[_]) extends ArrayValuesError {
  override def code: String = "fromNeg"
  override def reason = "the start of the range may be negative"
}
case class ArrayValuesFromToOrder(node: Values[_]) extends ArrayValuesError {
  override def code: String = "fromCrossesTo"
  override def reason = "the start of the range may exceed the end of the range"
}
case class ArrayValuesToLength(node: Values[_]) extends ArrayValuesError {
  override def code: String = "toLength"
  override def reason =
    "the end of the range may exceed the length of the array"
}
case class ArrayValuesPerm(node: Values[_]) extends ArrayValuesError {
  override def code: String = "valuesPerm"
  override def reason =
    "there may be insufficient permission to access the array at the specified range"
}

sealed trait PointerSubscriptError extends FrontendSubscriptError
sealed trait PointerDerefError extends PointerSubscriptError
sealed trait PointerLocationError extends PointerDerefError
sealed trait PointerAddError extends FrontendAdditiveError
case class PointerNull(node: Expr[_])
    extends PointerLocationError
    with PointerAddError
    with NodeVerificationFailure {
  override def code: String = "ptrNull"
  override def descInContext: String = "Pointer may be null."
  override def inlineDescWithSource(source: String): String =
    s"Pointer in `$source` may be null."
}
case class PointerBounds(node: Node[_])
    extends PointerSubscriptError
    with PointerAddError
    with NodeVerificationFailure {
  override def code: String = "ptrBlock"
  override def descInContext: String =
    "The offset to the pointer may be outside the bounds of the allocated memory area that the pointer is in."
  override def inlineDescWithSource(source: String): String =
    s"The offset in `$source` may be outside the bounds of the allocated memory area that the pointer is in."
}
case class PointerInsufficientPermission(node: Expr[_])
    extends PointerDerefError with NodeVerificationFailure {
  override def code: String = "ptrPerm"
  override def descInContext: String =
    "There may be insufficient permission to dereference the pointer."
  override def inlineDescWithSource(source: String): String =
    s"There may be insufficient permission to dereference `$source`."
}

sealed trait LockRegionFailure extends VerificationFailure

sealed trait LockFailure extends LockRegionFailure
case class LockObjectNull(node: Node[_])
    extends NodeVerificationFailure with LockFailure {
  override def code: String = "lockNull"
  override def descInContext: String = "Lock target may be null"
  override def inlineDescWithSource(source: String): String =
    s"Lock target in `$source` may be null."
}
case class LockNotCommitted(node: Lock[_])
    extends NodeVerificationFailure with LockFailure {
  override def code: String = "lockNotCommitted"
  override def descInContext: String =
    "Lock target may not yet have committed the lock invariant"
  override def inlineDescWithSource(source: String): String =
    s"Lock target in `$source` may not yet have committed the lock invariant."
}

sealed trait UnlockFailure extends LockRegionFailure
case class UnlockInvariantFailed(node: Unlock[_], failure: ContractFailure)
    extends UnlockFailure with WithContractFailure {
  override def baseCode: String = "invariantFailed"
  override def descInContext: String =
    "The lock invariant may not be exhaled here, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since $failure."
}
case class LockTokenNotHeld(node: Unlock[_], failure: ContractFailure)
    extends UnlockFailure with WithContractFailure {
  override def baseCode: String = "heldFailed"
  override def descInContext: String =
    "The token that indicates the lock is locked (`held(obj)`) may not be exhaled here, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since the `held` resource may not be exhaled here, since $failure."
}

sealed trait ConstructorFailure extends VerificationFailure
case class CommitFailed(node: Commit[_], failure: ContractFailure)
    extends ConstructorFailure with WithContractFailure {
  override def baseCode: String = "commitFailed"
  override def descInContext: String =
    "Committing the defined resources to the lock invariant may not be possible here, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may not commit the defined resources to the lock invariant, since $failure."
}

case class NotifyFailed(node: Notify[_], failure: ContractFailure)
    extends WithContractFailure {
  override def baseCode: String = "heldFailed"
  override def descInContext: String =
    "The token that indicated the lock is locked (`held(obj)`) may not be asserted here, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"`$node` may fail, since the token that indicated the lock is locked (`held(obj)`) may not be asserted here, since $failure."
}

case class ThrowNull(node: Throw[_]) extends NodeVerificationFailure {
  override def code: String = "null"
  override def descInContext: String = "The value thrown here may be null."
  override def inlineDescWithSource(source: String): String =
    s"`$source` may throw null."
}

case class ScaleNegative(node: Scale[_]) extends NodeVerificationFailure {
  override def code: String = "scaleNeg"
  override def descInContext: String = "The scale value here may be negative."
  override def inlineDescWithSource(source: String): String =
    s"The scale in `$source` may be negative."
}

case class NontrivialUnsatisfiable(node: ApplicableContract[_])
    extends NodeVerificationFailure {
  override def code: String = "unsatisfiable"
  override def descInContext: String =
    "The precondition of this contract may be unsatisfiable. If this is intentional, replace it with `requires false`."
  override def inlineDescWithSource(source: String): String =
    s"The precondition in `$source` may be unsatisfiable."
}

sealed trait UnsafeCoercion extends NodeVerificationFailure
case class CoerceRatZFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratZfrac"
  override def descInContext: String =
    "Rational may exceed the bounds of zfrac: [0, 1]"
  override def inlineDescWithSource(source: String): String =
    s"`$source` may exceed the bounds of zfrac: [0, 1]."
}
case class CoerceRatFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "ratFrac"
  override def descInContext: String =
    "Rational may exceed the bounds of frac: (0, 1]"
  override def inlineDescWithSource(source: String): String =
    s"`$source` may exceed the bounds of frac: (0, 1]."
}
case class CoerceZFracFracFailed(node: Expr[_]) extends UnsafeCoercion {
  override def code: String = "zfracFrac"
  override def descInContext: String = "zfrac may be zero."
  override def inlineDescWithSource(source: String): String =
    s"`$source` may be zero."
}

sealed trait JavaAnnotationFailure extends VerificationFailure

sealed trait BipConstructorFailure extends CallableFailure
sealed trait BipTransitionFailure extends CallableFailure

sealed trait BipTransitionContractFailure
    extends BipTransitionFailure with WithContractFailure {
  def transition: BipTransition[_]
  def failure: ContractFailure

  override final def node: Node[_] = transition.signature
  def signature: BipTransitionSignature[_] = transition.signature

  override def desc: String =
    Origin.messagesInContext(Seq(
      (signature.o, "In the following transition,"),
      (transition.o, s"with the following update function, $descInContext,"),
      (failure.node.o, failure.descCompletion + errUrl),
    ))
}

case class BipComponentInvariantNotEstablished(
    failure: ContractFailure,
    node: Procedure[_],
) extends BipConstructorFailure with WithContractFailure {
  override def baseCode: String = "bipComponentInvariantNotEstablished"
  override def descInContext: String =
    "In this constructor the component invariant is not established, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The component invariant cannot be established in $node, since $failure"
}

case class BipStateInvariantNotEstablished(
    failure: ContractFailure,
    node: Procedure[_],
) extends BipConstructorFailure with WithContractFailure {
  override def baseCode: String = "bipStateInvariantNotEstablished"
  override def descInContext: String =
    "In this constructor the invariant of the state is not established, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The state invariant is not established in $node, since $failure"
}

case class BipComponentInvariantNotMaintained(
    failure: ContractFailure,
    transition: BipTransition[_],
) extends BipTransitionContractFailure {
  override def baseCode: String = "bipComponentInvariantNotMaintained"
  override def descInContext: String =
    "the invariant of the component is not maintained, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The component invariant is not maintained in $node, since $failure"
}

case class BipStateInvariantNotMaintained(
    failure: ContractFailure,
    transition: BipTransition[_],
) extends BipTransitionContractFailure {
  override def baseCode: String = "bipStateInvariantNotMaintained"
  override def descInContext: String =
    "the invariant of the state is not maintained, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The state invariant is not maintained in $node, since $failure"
}

case class BipTransitionPostconditionFailure(
    failure: ContractFailure,
    transition: BipTransition[_],
) extends BipTransitionContractFailure {
  override def baseCode: String = "bipTransitionPostconditionFailure"
  override def descInContext: String = "the postcondition does not hold, since"
  override def inlineDescWithSource(node: String, failure: String): String =
    s"The postcondition of $node does not hold, since $failure"
}

case class BipTransitionPreconditionUnsatisfiable(node: BipTransition[_])
    extends BipTransitionFailure with NodeVerificationFailure {
  override def code: String = "bipTransitionPreconditionUnsatisfiable"
  override def descInContext: String =
    "The precondition of this transition is unsatisfiable"

  override def inlineDescWithSource(source: String): String =
    s"Precondition unsatisfiable for transition `$source`"
}

case class BipOutgoingDataPreconditionUnsatisfiable(node: BipOutgoingData[_])
    extends BipTransitionFailure with NodeVerificationFailure {
  override def code: String = "bipOutgoingDataPreconditionUnsatisfiable"
  override def descInContext: String =
    "The precondition of this outgoing data is unsatisfiable"

  override def inlineDescWithSource(source: String): String =
    s"Precondition unsatisfiable for outgoing data `$source`"
}

sealed trait BipGuardFailure extends CallableFailure
case class BipGuardPreconditionUnsatisfiable(node: BipGuard[_])
    extends BipGuardFailure with NodeVerificationFailure {
  override def code: String = "bipGuardPreconditionUnsatisfiable"
  override def descInContext: String =
    "The precondition of this guard (consisting of only the component invariant) is unsatisfiable"
  override def inlineDescWithSource(source: String): String =
    s"Precondition unsatisfiable for guard `$source`"
}

sealed trait BipGlueFailure extends VerificationFailure
sealed trait BipSynchronizationFailure extends BipGlueFailure
case class TransitionPreconditionFailed(
    synchronization: BipTransitionSynchronization[_],
    transition: BipTransition[_],
    failure: ContractFailure,
) extends BipSynchronizationFailure with WithContractFailure {

  override def node: Node[_] = transition.signature
  override def baseCode: String = "bipTransitionPreconditionFailed"
  override def desc: String =
    Origin.messagesInContext(
      (
        synchronization.o,
        s"In this context there is a synchronization, in which the following ${synchronization
            .transitions.size} transitions and ${synchronization.wires.size} data wires participate:",
      ) +:
        (synchronization.transitions.zipWithIndex.map { case (t, i) =>
          (t.decl.signature.o, s"transition ${i + 1},")
        } ++ synchronization.wires.zipWithIndex.map { case (w, i) =>
          (w.o, s"data wire ${i + 1},")
        }) :+
        (
          transition.signature.o,
          "the precondition of this transition does not hold, since",
        ) :+ (failure.node.o, s"${failure.descCompletion} $errUrl")
    )

  // Unused
  override def descInContext: String = ???

  override def inlineDescWithSource(node: String, failure: String): String =
    s"Precondition of $node does not hold, in a particular synchronization, since $failure"
}

trait Blame[-T <: VerificationFailure] {
  def blame(error: T): Unit
}

case class FilterExpectedErrorBlame(
    otherwise: Blame[VerificationFailure],
    expectedError: ExpectedError,
) extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit =
    if (expectedError.errorCode.r.matches(error.code)) {
      expectedError.trip(error)
    } else { otherwise.blame(error) }
}

case object BlamePathError extends SystemError {
  override def text: String =
    "The accounting for a pre- or postcondition is wrong: the path is empty before the layered blames are resolved, or an empty path was expected but it is not."
}

case object PostBlameSplit {
  def apply[T >: PostconditionFailed <: VerificationFailure](
      blames: Map[AccountedDirection, Blame[PostconditionFailed]],
      default: Blame[T],
  ): PostBlameSplit[T] = new PostBlameSplit(blames, default)

  def left[T >: PostconditionFailed <: VerificationFailure](
      left: Blame[PostconditionFailed],
      right: Blame[T],
  ): PostBlameSplit[T] =
    new PostBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right[T >: PostconditionFailed <: VerificationFailure](
      left: Blame[T],
      right: Blame[PostconditionFailed],
  ): PostBlameSplit[T] =
    new PostBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class PostBlameSplit[T >: PostconditionFailed <: VerificationFailure](
    blames: Map[AccountedDirection, Blame[PostconditionFailed]],
    default: Blame[T],
) extends Blame[T] {
  override def blame(error: T): Unit =
    error match {
      case PostconditionFailed(path, failure, invokable) =>
        path match {
          case Nil => throw BlamePathError
          case FailLeft +: tail =>
            blames(FailLeft)
              .blame(PostconditionFailed(tail, failure, invokable))
          case FailRight +: tail =>
            blames(FailRight)
              .blame(PostconditionFailed(tail, failure, invokable))
        }
      case other => default.blame(other)
    }
}

case object PreBlameSplit {
  def apply[T >: PreconditionFailed <: VerificationFailure](
      blames: Map[AccountedDirection, Blame[PreconditionFailed]],
      default: Blame[T],
  ): PreBlameSplit[T] = new PreBlameSplit(blames, default)

  def left[T >: PreconditionFailed <: VerificationFailure](
      left: Blame[PreconditionFailed],
      right: Blame[T],
  ): PreBlameSplit[T] =
    new PreBlameSplit(Map(FailLeft -> left, FailRight -> right), right)

  def right[T >: PreconditionFailed <: VerificationFailure](
      left: Blame[T],
      right: Blame[PreconditionFailed],
  ): PreBlameSplit[T] =
    new PreBlameSplit(Map(FailLeft -> left, FailRight -> right), left)
}

case class PreBlameSplit[T >: PreconditionFailed <: VerificationFailure](
    blames: Map[AccountedDirection, Blame[PreconditionFailed]],
    default: Blame[T],
) extends Blame[T] {
  override def blame(error: T): Unit =
    error match {
      case PreconditionFailed(path, failure, invokable) =>
        path match {
          case Nil => throw BlamePathError
          case FailLeft +: tail =>
            blames(FailLeft).blame(PreconditionFailed(tail, failure, invokable))
          case FailRight +: tail =>
            blames(FailRight)
              .blame(PreconditionFailed(tail, failure, invokable))
        }
      case other => default.blame(other)
    }
}

case class BlameUnreachable(message: String, failure: VerificationFailure)
    extends VerificationError.SystemError {
  def text: String =
    s"An error condition was reached, which should be statically unreachable. $message. Inner failure:\n${failure
        .toString.split('\n').mkString(" > ", "\n > ", "")}"
}

case class PanicBlame(message: String) extends Blame[VerificationFailure] {
  override def blame(error: VerificationFailure): Unit =
    throw BlameUnreachable(message, error)
}

object NeverNone
    extends PanicBlame(
      "get in `opt == none ? _ : get(opt)` should always be ok."
    )
object FramedSeqIndex
    extends PanicBlame(
      "access in `∀i. 0 <= i < |xs| ==> ...xs[i]...` should never be out of bounds"
    )
object FramedArrIndex
    extends PanicBlame(
      "access in `∀i. 0 <= i < xs.length ==> Perm(xs[i], read) ** ...xs[i]...` should always be ok"
    )
object IteratedArrayInjective
    extends PanicBlame(
      "access in `∀*i. 0 <= i < xs.length ==> Perm(xs[i], _)` should always be injective"
    )
object IteratedPtrInjective
    extends PanicBlame(
      "access in `∀*i. Perm(xs[i], _)` should always be injective"
    )
object FramedArrLoc extends PanicBlame("Bounds and non-nullness are ensured.")
object FramedArrLength
    extends PanicBlame(
      "length query in `arr == null ? _ : arr.length` should always be ok."
    )
object FramedPtrBlockLength
    extends PanicBlame(
      "length query in `p == null ? _ : \\pointer_block_length(p)` should always be ok."
    )
object FramedPtrBlockOffset
    extends PanicBlame(
      "offset query in `p == null ? _ : \\pointer_block_offset(p)` should always be ok."
    )
object FramedMapGet
    extends PanicBlame(
      "access in `∀k. k \\in m.keys ==> ...m[k]...` should always be ok."
    )
object FramedGetLeft
    extends PanicBlame("left in `e.isLeft ? e.left : ...` should always be ok.")
object FramedGetRight
    extends PanicBlame(
      "right in `e.isLeft ? ... : e.right` should always be ok."
    )
object AbstractApplicable
    extends PanicBlame(
      "the postcondition of an abstract applicable is not checked, and hence cannot fail."
    )
object TriggerPatternBlame
    extends PanicBlame(
      "patterns in a trigger are not evaluated, but schematic, so any blame in a trigger is never applied."
    )
object TrueSatisfiable
    extends PanicBlame("`requires true` is always satisfiable.")
object FramedPtrOffset
    extends PanicBlame(
      "pointer arithmetic in (0 <= \\pointer_block_offset(p)+i < \\pointer_block_length(p)) ? p+i : _ should always be ok."
    )
object FramedByForPerm
    extends PanicBlame(
      "Heap value access should be ok inside a forperm that frames it."
    )

object AssignLocalOk extends PanicBlame("Assigning to a local can never fail.")
object DerefAssignTarget
    extends PanicBlame(
      "Assigning to a field should trigger an error on the assignment, and not on the dereference."
    )
object SubscriptAssignTarget
    extends PanicBlame(
      "Assigning to a subscript should trigger an error on the assignment, and not on the subscript."
    )
object DerefPerm
    extends PanicBlame(
      "Dereferencing a field in a permission should trigger an error on the permission, not on the dereference."
    )
object ArrayPerm
    extends PanicBlame(
      "Subscripting an array in a permission should trigger an error on the permission, not on the dereference."
    )
object UnresolvedDesignProblem
    extends PanicBlame(
      "The design does not yet accommodate passing a meaningful blame here."
    )
object PointsToDeref
    extends PanicBlame(
      "The permission has already been ensured and thus cannot be insufficient."
    )

object JavaArrayInitializerBlame
    extends PanicBlame(
      "The explicit initialization of an array in Java should never generate an assignment that exceeds the bounds of the array"
    )

object UnsafeDontCare {
  case class Satisfiability(reason: String)
      extends UnsafeDontCare[NontrivialUnsatisfiable]
}

trait UnsafeDontCare[T <: VerificationFailure]
    extends Blame[T] with LazyLogging {
  def reason: String

  override def blame(error: T): Unit = {
    logger.debug(s"We do not care about ${error.code} here, since $reason:")
    logger.debug(error.toString)
  }
}

case class NoContext(inner: Blame[PreconditionFailed])
    extends Blame[InvocationFailure] {
  override def blame(error: InvocationFailure): Unit =
    error match {
      case pre: PreconditionFailed => inner.blame(pre)
      case ctx: ContextEverywhereFailedInPre =>
        PanicBlame(
          "Function or method does not list any context_everywhere clauses, so cannot fail on a context_everywhere clause."
        ).blame(ctx)
    }
}

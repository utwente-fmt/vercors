package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
import vct.col.ast.{CPPLocalDeclaration, Expr, FunctionInvocation, InstanceField, Perm, ProcedureInvocation, SeqSubscript, _}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.NotApplicable
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.rewrite.ParBlockEncoder.ParBlockNotInjective
import vct.col.rewrite.{Generation, ParBlockEncoder, Rewritten}
import vct.col.util.AstBuildHelpers.{assignLocal, _}
import vct.col.util.{AstBuildHelpers, SuccessionMap}
import vct.result.Message
import vct.result.VerificationError.{Unreachable, UserError}
import vct.rewrite.lang.LangSpecificToCol.NotAValue

import scala.collection.mutable

case object LangCPPToCol {

  private case class WrongCPPType(decl: CPPLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongCPPType"
    override def text: String =
      decl.o.messageInContext(s"This declaration has a type that is not supported.")
  }

  private case class UnexpectedCPPTypeError(declType: Type[_], init: Expr[_]) extends UserError {
    override def code: String = "unexpectedCPPTypeError"
    override def text: String =
      init.o.messageInContext(s"Expected the type of this expression to be `$declType`, but got ${init.t}")
  }

  private case class LambdaDefinitionUnsupported(lambda: CPPLambdaDefinition[_]) extends UserError {
    override def text: String = lambda.o.messageInContext("Lambda expressions are only supported as parameters for invocations of SYCL's submit and parallel_for methods.")
    override def code: String = "unsupportedLambdaDefinition"
  }

  private case class CPPDoubleContracted(decl: CPPGlobalDeclaration[_], defn: CPPFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"
    override def text: String =
      Message.messagesInContext(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      )
  }

  private abstract class CPPInvocationBlame(kind: String) extends Blame[InvocationFailure] {
    def preconditionFailed(error: PreconditionFailed): Unit

    override def blame(error: InvocationFailure): Unit = error match {
      case predErr@PreconditionFailed(_, _, _) => preconditionFailed(predErr)
      case ContextEverywhereFailedInPre(_, _) => PanicBlame(s"${kind.capitalize} do not contain context_everywhere clauses, so cannot fail on a context_everywhere clause.").blame(error)
    }
  }

  private case class ImpossibleDivByZeroBlame() extends Blame[DivByZero] {
    override def blame(error: DivByZero): Unit = error.node.o match {
      case o if o.find[NDRangeDimensionCheck.type].isDefined => PanicBlame(o.messageInContext(
        "Division by zero is possible when dividing the global range dimension " +
          "by the local range dimension, but it was already asserted that this is actually impossible."
      )).blame(error)
      case _ => PanicBlame("Division by zero is possible here, but it is actually impossible.").blame(error)
    }
  }

  private case class KernelLambdaRunMethodBlame(kernelLambda: CPPLambdaDefinition[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case PostconditionFailed(path, failure, node) =>
        kernelLambda.blame.blame(SYCLKernelLambdaFailure(KernelPostconditionFailed(failure, Right(kernelLambda))))
      case TerminationMeasureFailed(applicable, apply, measure) =>
        PanicBlame("Kernel lambdas do not have a termination measure yet").blame(error)
      case ContextEverywhereFailedInPost(failure, node) =>
        PanicBlame("Kernel lambdas do not have context_everywhere specifications").blame(error)
      case SignalsFailed(failure, node) =>
        PanicBlame("Kernel lambdas cannot throw exceptions").blame(error)
      case ExceptionNotInSignals(node) =>
        PanicBlame("Kernel lambdas cannot throw exceptions").blame(error)
    }
  }

  private case class SYCLReassigningOfVariableUnsupported(objectName: String, codeExtension: String, ass: PreAssignExpression[_]) extends UserError {
    override def code: String = "syclUnsupportedReassigningOf" + codeExtension
    override def text: String = ass.o.messageInContext(s"Reassigning variables holding a SYCL " + objectName + " is not supported.")
  }

  private case class SYCLReassigningOfReadonlyAccessor(ass: PreAssignExpression[_]) extends UserError {
    override def code: String = "syclUnsupportedReassigningOfReadonlyAccessor"
    override def text: String = ass.o.messageInContext(s"Reassigning (an element of) a readonly data accessor is not allowed.")
  }

  private case class SYCLContractForCommandGroupUnsupported(contract: ApplicableContract[_]) extends UserError {
    override def code: String = "syclContractForCommandGroupUnsupported"
    override def text: String = contract.o.messageInContext(s"Contracts above SYCL command groups are not supported.")
  }

  private case class SYCLGivenYieldsOnSYCLMethodsUnsupported(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "syclGivenYieldsOnSYCLMethodsUnsupported"
    override def text: String = inv.o.messageInContext(s"Given and yields annotations are not supported for calls to built-in SYCL methods.")
  }

  private case class SYCLNoMultipleKernels(decl1: Statement[_], decl2: CPPInvocation[_]) extends UserError {
    override def code: String = "syclNoMultipleKernels"
    override def text: String = Message.messagesInContext(
      decl2.o -> "This kernel declaration is not allowed, as only one kernel declaration is allowed per command group, ...",
      decl1.o -> "... and there is already a kernel declared here.",
    )
  }

  private case class SYCLMissingKernel(decl: Statement[_]) extends UserError {
    override def code: String = "syclMissingKernel"
    override def text: String = decl.o.messageInContext(s"There is no kernel declaration in this command group.")
  }

  private case class SYCLNoExtraCodeInCommandGroup(decl: Statement[_]) extends UserError {
    override def code: String = "syclNoExtraCodeInCommandGroup"
    override def text: String = decl.o.messageInContext(s"Non-SYCL code inside command group declarations is not supported.")
  }

  private case class SYCLIncorrectParallelForLambdaArgument(o: Origin) extends UserError {
    override def code: String = "syclIncorrectParallelForLambdaArgument"
    override def text: String = o.messageInContext("The second parameter for the parallel_for method, which is a lambda method, " +
      "only takes one (nd_)item argument, which should match the first (nd_)range parameter of the parallel_for method.")
  }

  private case class SYCLItemMethodInvocationBlame(inv: CPPInvocation[_]) extends CPPInvocationBlame("SYCL item methods") {
    override def preconditionFailed(error: PreconditionFailed): Unit = inv.blame.blame(SYCLItemMethodPreconditionFailed(error.node))
  }

  private case class SYCLItemMethodSeqBoundFailureBlame(inv: CPPInvocation[_]) extends Blame[SeqBoundFailure] {
    private case class SYCLItemMethodSeqBoundNegativeError() extends UserError {
      override def code: String = "syclItemMethodSeqBoundNegative"
      override def text: String = inv.o.messageInContext("The dimension parameter may not be negative.")
    }

    private case class SYCLItemMethodSeqBoundExceedsLengthError() extends UserError {
      override def code: String = "syclItemMethodSeqBoundExceedsLength"
      override def text: String = inv.o.messageInContext("The dimension parameter should be smaller than the number of dimensions in the (nd_)item..")
    }

    override def blame(error: SeqBoundFailure): Unit = error match {
      case SeqBoundNegative(_) => throw SYCLItemMethodSeqBoundNegativeError()
      case SeqBoundExceedsLength(_) => throw SYCLItemMethodSeqBoundExceedsLengthError()
    }
  }

  private case class SYCLKernelForkNull(node: Fork[_]) extends UserError {
    override def code: String = "syclKernelForkNull"
    override def text: String = node.o.messageInContext("This event variable might not be linked to a kernel submission to a queue.")
  }

  private case object RangeDimensionCheck extends OriginContent
  private case object NDRangeDimensionCheck extends OriginContent

  private def RangeDimensionCheckOrigin(o: Origin): Origin =
    o
      .where(name = "RangeDimensionCheck")
      .withContent(RangeDimensionCheck)

  private def NDRangeDimensionCheckOrigin(o: Origin, dimension: Option[Int] = None): Origin =
    o
      .where(name = "NDRangeDimensionCheck", context = s"range dimension ${dimension.getOrElse("?")}")
      .withContent(NDRangeDimensionCheck)

  private case class SYCLKernelPreconditionNotEstablished(error: WithContractFailure) extends UserError {
    override def code: String = "syclKernelForkPre"
    override def text: String =  (error.failure, error.failure.node.o) match {
      case (ContractFalse(_), o) if o.find[RangeDimensionCheck.type].isDefined =>
        o.messageInContext("All range dimensions should be greater or equal to zero.")
      case (ContractFalse(_), o) if o.find[NDRangeDimensionCheck.type].isDefined =>
        o.messageInContext(
          "Every global range dimension should be divisible by the local range dimension at the same index," +
          " and the local range dimension should be greater than 0 to avoid division by zero. " +
          "All global range dimensions should be greater or equal to zero."
        )
      case _ => Message.messagesInContext((error.node.o, "The precondition of the kernel may not hold, since ..."), (error.failure.node.o, "... " + error.failure.descCompletion))
    }
  }

  private case class SYCLKernelForkBlame(kernel: CPPLambdaDefinition[_]) extends Blame[ForkFailure] {
    override def blame(error: ForkFailure): Unit = error match {
      case ForkNull(node) => throw SYCLKernelForkNull(node)
      case RunnableNotIdle(_) => PanicBlame("This kernel submission has been forked already. This should not happen.").blame(error)
      case e@RunnablePreconditionNotEstablished(_, _) => throw SYCLKernelPreconditionNotEstablished(e)
    }
  }

  private case class SYCLKernelJoinBlame() extends Blame[JoinFailure] {
    private case class KernelJoinError(error: JoinFailure) extends UserError {
      override def code: String = "kernelJoinNull"
      override def text: String = "This event variable might be null or no longer be linked to a kernel submission to a queue."
    }
    override def blame(error: JoinFailure): Unit = throw KernelJoinError(error)
  }

  private case class SYCLKernelParBlockFailureBlame(kernel: CPPLambdaDefinition[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
      case ParPredicateNotInjective(_, predicate) =>
        kernel.blame.blame(SYCLKernelLambdaFailure(KernelPredicateNotInjective(Right(kernel), predicate)))
      case preFailed@ParPreconditionFailed(_, _) =>
        throw SYCLKernelPreconditionNotEstablished(preFailed)
      case ParBlockPostconditionFailed(failure, _) =>
        kernel.blame.blame(SYCLKernelLambdaFailure(KernelPostconditionFailed(failure, Right(kernel))))
      case ParBlockMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
    }
  }

  private case class SYCLKernelRangeInvalidBlame() extends CPPInvocationBlame("SYCL kernel constructors") {
    private case class KernelRangeInvalidError(error: PreconditionFailed) extends UserError {
      override def code: String = "syclKernelRangeInvalid"

      override def text: String = error.failure.node.o match {
        case o if o.find[RangeDimensionCheck.type].isDefined => o.messageInContext("All range dimensions should be greater or equal to zero.")
        case o if o.find[NDRangeDimensionCheck.type].isDefined => o.messageInContext(
          "Every global range dimension should be divisible by the local range dimension at the same index," +
            " and the local range dimension should be greater than 0 to avoid division by zero. " +
            "All global range dimensions should be greater or equal to zero.")
        case _ => Message.messagesInContext((error.node.o, "Precondition may not hold, since ..."), (error.failure.node.o, "... " + error.failure.descCompletion))
      }
    }

    def preconditionFailed(error: PreconditionFailed): Unit = throw KernelRangeInvalidError(error)
  }

  private case class SYCLBufferConstructionFailed(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "syclBufferConstructionFailed"

    override def text: String = inv.o.messageInContext(
      "This buffer cannot be constructed because there is insufficient permission to copy and exclusively claim the hostData. " +
      "Write permission to the hostData over the entire bufferRange is required."
    )
  }

  private case class SYCLBufferConstructionInvocationBlame(inv: CPPInvocation[_]) extends CPPInvocationBlame("SYCL buffer constructors") {
    override def preconditionFailed(error: PreconditionFailed): Unit = throw SYCLBufferConstructionFailed(inv)
  }

  private case class SYCLBufferConstructionFoldFailedBlame(inv: CPPInvocation[_]) extends Blame[FoldFailed] {
    override def blame(error: FoldFailed): Unit = throw SYCLBufferConstructionFailed(inv)
  }

  private case class SYCLBufferDestructionFailed(bufferDecl: Variable[_], scope: CPPLifetimeScope[_]) extends UserError {
    override def code: String = "syclBufferDestructionFailed"
    override def text: String = Message.messagesInContext(
      bufferDecl.o -> "This buffer cannot be destroyed at the end of this scope, ...",
      scope.o -> "... because the contents of the buffer cannot be copied back to the hostData memory due to insufficient permission to the hostData at the end of this scope.",
    )
  }

  private case class SYCLBufferDestructionInvocationBlame(bufferDecl: Variable[_], scope: CPPLifetimeScope[_]) extends CPPInvocationBlame("SYCL buffer destructors") {
    override def preconditionFailed(error: PreconditionFailed): Unit = throw SYCLBufferDestructionFailed(bufferDecl, scope)
  }

  private case class SYCLBufferDestructionUnfoldFailedBlame(bufferDecl: Variable[_], scope: CPPLifetimeScope[_]) extends Blame[UnfoldFailed] {
    override def blame(error: UnfoldFailed): Unit = throw SYCLBufferDestructionFailed(bufferDecl, scope)
  }

  private case class SYCLAccessorFieldInsufficientReferencePermissionBlame(local: CPPLocal[_]) extends Blame[InsufficientPermission] {
    private case class SYCLAccessorFieldInsufficientReferencePermissionError(error: InsufficientPermission) extends UserError {
      override def code: String = "syclAccessorFieldInsufficientReferencePermission"
      override def text: String = local.o.messageInContext(error.descInContext)
    }

    override def blame(error: InsufficientPermission): Unit = throw SYCLAccessorFieldInsufficientReferencePermissionError(error)
  }

  private case class SYCLAccessorRangeIndexFieldInsufficientReferencePermissionBlame(inv: CPPInvocation[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = PanicBlame(inv.o.messageInContext(s"There was not enough permission to access" +
      s" a field containing the size of an accessor dimension. This should not be possible.")).blame(error)
  }

  private case class SYCLRequestedRangeIndexOutOfBoundsBlame(seq: LiteralSeq[_], index: Expr[_]) extends Blame[SeqBoundFailure] {
    private case class SYCLRangeIndexOutOfBoundsError(error: SeqBoundFailure) extends UserError {
      override def code: String = "syclAccessorInsufficientReferencePermission"
      override def text: String = index.o.messageInContext(s"This parameter is out of bounds, it should be an integer between 0 (inclusive) and ${seq.values.size} (exclusive).")
    }

    override def blame(error: SeqBoundFailure): Unit = throw SYCLRangeIndexOutOfBoundsError(error)
  }

  private case class SYCLWrongNumberOfSubscriptsForAccessor(sub: AmbiguousSubscript[_], numberOfSubs: Int, expectedNumberOfSubs: Int) extends UserError {
    override def code: String = "syclWrongNumberOfSubscriptsForAccessor"
    override def text: String = sub.o.messageInContext(s"Only fully subscripting accessors is allowed. " +
      s"The current subscripts are ${"[...]".repeat(numberOfSubs)}, but expected ${"[...]".repeat(expectedNumberOfSubs)}.")
  }

  private case class SYCLKernelRunMethodContractUnsatisfiableBlame(preCondition: UnitAccountedPredicate[_]) extends Blame[NontrivialUnsatisfiable] {
    override def blame(error: NontrivialUnsatisfiable): Unit = error.node.o match {
      case o if o.find[SYCLGeneratedAccessorPermissions.type].isDefined =>
        PanicBlame(o.messageInContext("Generated permissions for this accessor are unsatisfiable, but this should not be possible.")).blame(error)
      case _ => preCondition.o.blame(error)
    }
  }

  private case class SYCLKernelConstructorCallableFailureBlame() extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case PostconditionFailed(_, _, node) if node.o.find[SYCLGeneratedAccessorPermissions.type].isDefined =>
        PanicBlame(node.o.messageInContext("Generated post conditions for this accessor are unsatisfiable, but this should not be possible.")).blame(error)
      case p@PostconditionFailed(_, failure, node) => Message.messagesInContext((node.o, p.descInContext + ", since ..."), (failure.node.o, "... " + failure.descCompletion))
      case ContextEverywhereFailedInPost(_, node) if node.o.find[SYCLGeneratedAccessorPermissions.type].isDefined =>
        PanicBlame(node.o.messageInContext("Generated post conditions for this accessor are unsatisfiable, but this should not be possible.")).blame(error)
      case c@ContextEverywhereFailedInPost(failure, node) => Message.messagesInContext((node.o, c.descInContext + ", since ..."), (failure.node.o, "... " + failure.descCompletion))
      case TerminationMeasureFailed(_, _, _) => PanicBlame("This kernel class constructor should always be able to terminate.").blame(error)
      case SignalsFailed(_, _) | ExceptionNotInSignals(_) => PanicBlame("This kernel class constructor contains no signals clause.").blame(error)
    }
  }

  private case class SYCLKernelConstructorNontrivialUnsatisfiableBlame(commandGroupO: Origin) extends Blame[NontrivialUnsatisfiable] {
    private case class SYCLKernelConstructorNontrivialUnsatisfiableError() extends UserError {
      override def code: String = "syclKernelConstructorNontrivialUnsatisfiable"
      override def text: String = commandGroupO.messageInContext(s"The precondition of the contract of the kernel constructor generated for this command group may be unsatisfiable. If this is intentional, replace it with `requires false`.")
    }

    override def blame(error: NontrivialUnsatisfiable): Unit = throw SYCLKernelConstructorNontrivialUnsatisfiableError()
  }

  private class SYCLAccessorDerefBlame(accField: InstanceField[_]) extends PanicBlame(accField.o.messageInContext("There should always be enough permission to dereference the accessor field."))

  private class SYCLAccessorDimensionDerefBlame(accDimField: InstanceField[_]) extends PanicBlame(accDimField.o.messageInContext("There should always be enough permission to dereference the range dimension field of an accessor."))

  private class SYCLRangeDerefBlame(rangeField: InstanceField[_]) extends PanicBlame(rangeField.o.messageInContext("There should always be enough permission to dereference the range fields."))

  private case class SYCLAccessorArraySubscriptErrorBlame(accSubscript: AmbiguousSubscript[_]) extends Blame[ArraySubscriptError] {
    private case class SYCLAccessorArraySubscriptArrayInsufficientPermissionError(error: ArrayInsufficientPermission) extends UserError {
      override def code: String = "syclAccessorArraySubscriptArrayInsufficientPermission"
      override def text: String = accSubscript.o.messageInContext(error.descInContext)
    }

    private case class SYCLAccessorArraySubscriptArrayBoundsError(error: ArrayBounds) extends UserError {
      override def code: String = "syclAccessorArraySubscriptArrayBounds"
      override def text: String = accSubscript.o.messageInContext(error.descInContext)
    }

    override def blame(error: ArraySubscriptError): Unit = error match {
      case a@ArrayInsufficientPermission(_) => throw SYCLAccessorArraySubscriptArrayInsufficientPermissionError(a)
      case ArrayNull(_) => PanicBlame(accSubscript.o.messageInContext("It is already stated in generated contracts that the accessor array is not null.")).blame(error)
      case a@ArrayBounds(_) => throw SYCLAccessorArraySubscriptArrayBoundsError(a)
    }
  }

  private case class SYCLAccessorArraySubscriptLinearizeInvocationBlame(accSubscript: AmbiguousSubscript[_], accessorRef: CPPLocal[_], indices: Seq[Expr[_]]) extends Blame[InvocationFailure] {
    private case class SYCLAccessorArraySubscriptLinearizePreconditionBlame() extends Blame[PreconditionFailed] {
      private case class SYCLAccessorArraySubscriptLinearizePreconditionError() extends UserError {
        override def code: String = "syclAccessorArraySubscriptLinearizePreconditionFailed"
        override def text: String = accSubscript.o.messageInContext("One or more of the indices may be outside the bounds on the accessor. " +
          "Try adding pre-conditions which state that the indices are within the accessor bounds, such as:\n" +
          Seq.range(0, indices.size).map(i => s"context ${indices(i)} < ${accessorRef.name}.get_range().get($i);").mkString("\n"))
      }

      override def blame(error: PreconditionFailed): Unit = throw SYCLAccessorArraySubscriptLinearizePreconditionError()
    }

    override def blame(error: InvocationFailure): Unit = NoContext(SYCLAccessorArraySubscriptLinearizePreconditionBlame()).blame(error)
  }

  private case class SYCLBufferOutOfScopeError(accDecl: CPPLocalDeclaration[_]) extends UserError {
    override def code: String = "syclBufferOutOfScope"
    override def text: String = accDecl.o.messageInContext("The buffer has to be declared in the same or higher scope as the accessor declaration.")
  }

  private case class SYCLPredicateFoldingNotAllowed(pred: Expr[_]) extends UserError {
    override def code: String = "syclPredicateFoldingNotAllowed"

    override def text: String = pred.o.messageInContext("This predicate is used internally and is thus not allowed to be (un)folded.")
  }

  private case class SYCLNoLocalAccessorsInBasicKernel(local: CPPLocal[_]) extends UserError {
    override def code: String = "syclNoLocalAccessorsInBasicKernel"
    override def text: String = local.o.messageInContext("Local accessors cannot be used in basic kernels.")
  }

  private case class SYCLLocalAccessorArraySizeBlame(range: Expr[_]) extends Blame[ArraySizeError] {
    private case class SYCLLocalAccessorArraySizeError(error: ArraySizeError) extends UserError {
      override def code: String = "syclLocalAccessorArraySizeError"

      override def text: String = range.o.messageInContext("One or more of the dimensions of the local accessor's range may be negative.")
    }

    override def blame(error: ArraySizeError): Unit = throw SYCLLocalAccessorArraySizeError(error)
  }

  private case object SYCLGeneratedAccessorPermissions extends OriginContent

  private def SYCLGeneratedAccessorPermissionsOrigin(accessor: CPPLocalDeclaration[_]): Origin =
    accessor.o.where(context = "accessor permissions").withContent(SYCLGeneratedAccessorPermissions)
}

case class LangCPPToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangCPPToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val cppFunctionSuccessor: SuccessionMap[CPPFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()
  val cppFunctionDeclSuccessor: SuccessionMap[(CPPGlobalDeclaration[Pre], Int), Procedure[Post]] = SuccessionMap()
  val cppNameSuccessor: SuccessionMap[CPPNameTarget[Pre], Variable[Post]] = SuccessionMap()
  val cppGlobalNameSuccessor: SuccessionMap[CPPNameTarget[Pre], HeapVariable[Post]] = SuccessionMap()
  val cppCurrentDefinitionParamSubstitutions: ScopedStack[Map[CPPParam[Pre], CPPParam[Pre]]] = ScopedStack()

  val syclHelperFunctions: mutable.Map[String, (Seq[Expr[Post]], Blame[InvocationFailure], Origin) => FunctionInvocation[Post]] = mutable.Map.empty
  val syclBufferTypeToHostPredicateAndProcedures: mutable.Map[Type[Post], (Predicate[Post], Procedure[Post], Procedure[Post])] = mutable.Map.empty
  val syclRangeVariableToInitializer: mutable.Map[Variable[Post], Expr[Post]] = mutable.Map.empty

  val syclBufferSuccessor: ScopedStack[mutable.Map[Variable[Post], SYCLBuffer[Post]]] = ScopedStack()
  val syclAccessorSuccessor: mutable.Map[CPPNameTarget[Pre], SYCLAccessor[Post]] = mutable.Map.empty
  val syclLocalAccessorVarToDimensions: mutable.Map[Variable[Post], Seq[Expr[Post]]] = mutable.Map.empty

  val currentlyRunningKernels: mutable.Map[Local[Post], Seq[SYCLAccessor[Post]]] = mutable.Map.empty
  val currentDimensionIterVars: mutable.Map[KernelScopeLevel, mutable.Buffer[IterVariable[Post]]] = mutable.Map.empty
  var currentKernelType: Option[KernelType] = None
  var currentThis: Option[Expr[Post]] = None

  sealed abstract class KernelScopeLevel(val idName: String)
  private case class GlobalScope() extends KernelScopeLevel("GLOBAL_ID")
  private case class LocalScope() extends KernelScopeLevel("LOCAL_ID")
  private case class GroupScope() extends KernelScopeLevel("GROUP_ID")

  sealed abstract class KernelType(rangeFields: Seq[InstanceField[Post]], rangeValues: Seq[Expr[Post]]) {
    def getRangeSizeChecksForConstructor(params: mutable.Buffer[Variable[Post]]): Expr[Post]
    def getConstructorPostConditions(result: Result[Post], params: mutable.Buffer[Variable[Post]]): Seq[Expr[Post]]

    def getRangeFields: Seq[InstanceField[Post]] = rangeFields
    def getRangeValues: Seq[Expr[Post]] = rangeValues
    def getRangeFieldPermissions(thisObj: Expr[Post]): Seq[Expr[Post]] = rangeFields.map(f => {
      implicit val o: Origin = f.o
      Star(Perm[Post](FieldLocation[Post](thisObj, f.ref), ReadPerm()), Deref[Post](thisObj, f.ref)(new SYCLRangeDerefBlame(f)) >= c_const(0))
    })
  }
  private case class BasicKernel(rangeO: Origin, rangeFields: Seq[InstanceField[Post]], rangeValues: Seq[Expr[Post]]) extends KernelType(rangeFields, rangeValues) {
    override def getRangeSizeChecksForConstructor(params: mutable.Buffer[Variable[Post]]): Expr[Post] = foldStar(params.indices.map(i => {
      implicit val o: Origin = RangeDimensionCheckOrigin(params(i).o)
      Local[Post](params(i).ref) >= c_const(0)
    }))(RangeDimensionCheckOrigin(rangeO))

    override def getConstructorPostConditions(result: Result[Post], params: mutable.Buffer[Variable[Post]]): Seq[Expr[Post]] = params.indices.map(i => {
      implicit val o: Origin = params(i).o
      Star(
        Perm(FieldLocation[Post](result, rangeFields(i).ref), ReadPerm()),
        Deref[Post](result, rangeFields(i).ref)(new SYCLRangeDerefBlame(rangeFields(i))) === Local[Post](params(i).ref)
      )
    })
  }
  private case class NDRangeKernel(rangeO: Origin, rangeFields: Seq[InstanceField[Post]], rangeValues: Seq[Expr[Post]]) extends KernelType(rangeFields, rangeValues) {
    override def getRangeSizeChecksForConstructor(params: mutable.Buffer[Variable[Post]]): Expr[Post] = {
      // Order of rangeFields is group0, local0, group1, local1, ...
      // Order of params is global0, local0, global1, local1, ...
      foldStar(Seq.range(0, params.size, 2).map(i => {
        implicit val o: Origin = NDRangeDimensionCheckOrigin(rangeO, Some(i))
          (Local[Post](params(i).ref) >= c_const(0)) &&
          (Local[Post](params(i+1).ref) > c_const(0)) &&
          (Mod(Local[Post](params(i).ref), Local[Post](params(i+1).ref))(ImpossibleDivByZeroBlame()) === c_const(0))
      }))(NDRangeDimensionCheckOrigin(rangeO))
    }

    override def getConstructorPostConditions(result: Result[Post], params: mutable.Buffer[Variable[Post]]): Seq[Expr[Post]] = {
      // Order of rangeFields is group0, local0, group1, local1, ...
      // Order of params is global0, local0, global1, local1, ...
      Seq.range(0, rangeFields.size, 2).map(i => {
        implicit val o: Origin = params(i).o
        foldStar(Seq(
          Perm(FieldLocation[Post](result, rangeFields(i).ref), ReadPerm()),
          Deref[Post](result, rangeFields(i).ref)(new SYCLRangeDerefBlame(rangeFields(i))) === FloorDiv(Local[Post](params(i).ref), Local[Post](params(i+1).ref))(ImpossibleDivByZeroBlame()),
          Perm(FieldLocation[Post](result, rangeFields(i+1).ref), ReadPerm()),
          Deref[Post](result, rangeFields(i+1).ref)(new SYCLRangeDerefBlame(rangeFields(i+1))) === Local[Post](params(i+1).ref),
          Deref[Post](result, rangeFields(i).ref)(new SYCLRangeDerefBlame(rangeFields(i))) * Deref[Post](result, rangeFields(i+1).ref)(new SYCLRangeDerefBlame(rangeFields(i+1))) === Local[Post](params(i).ref)
        ))}
      )
    }
  }

  case class SYCLBuffer[Post](hostData: Expr[Post], generatedVar: Variable[Post], range: SYCLRange[Post], typ: SYCLTBuffer[Post])(implicit val o: Origin)
  case class SYCLAccessor[Post](buffer: SYCLBuffer[Post], accessMode: SYCLAccessMode[Post], instanceField: InstanceField[Post], rangeIndexFields: Seq[InstanceField[Post]])(implicit val o: Origin)

  private def getFromAll[K, V](stack: ScopedStack[mutable.Map[K, V]], key: K): Option[V] = {
    stack.foreach(map => { if (map.contains(key)) return Some(map(key)) })
    None
  }

  def rewriteUnit(cppUnit: CPPTranslationUnit[Pre]): Unit = {
    cppUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cppParam: CPPParam[Pre]): Unit = {
    cppParam.drop()
    val varO = cppParam.o.where(name = CPP.getDeclaratorInfo(cppParam.declarator).name)

    val v = new Variable[Post](cppParam.specifiers.collectFirst
      { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get)(varO)
    cppNameSuccessor(RefCPPParam(cppParam)) = v
    rw.variables.declare(v)
  }

  def rewriteLambdaDefinition(lambda: CPPLambdaDefinition[Pre]): Expr[Post] = {
    throw LambdaDefinitionUnsupported(lambda)
  }

  def checkPredicateFoldingAllowed(predRes: Expr[Pre]): Unit = predRes match {
    case CPPInvocation(CPPLocal("sycl::buffer::exclusive_hostData_access", Seq()), _, _, _) => throw SYCLPredicateFoldingNotAllowed(predRes)
    case _ =>
  }

  def storeIfSYCLFunction(f: Function[Pre]): Unit = {
    if (f.o.getPreferredNameOrElse().snake.startsWith("sycl")) {
      syclHelperFunctions.put(
        f.o.getPreferredNameOrElse().snake,
        (args: Seq[Expr[Post]], blame: Blame[InvocationFailure], o: Origin) => FunctionInvocation[Post](rw.succ(f), args, Nil, Nil, Nil)(blame)(o)
      )
    }
  }

  def rewriteFunctionDef(func: CPPFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = CPP.getDeclaratorInfo(func.declarator)
    val params = info.params.get
    val returnType = func.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get

    val (contract, subs: Map[CPPParam[Pre], CPPParam[Pre]]) = func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) if decl.decl.contract.nonEmpty =>
        if (func.contract.nonEmpty) throw CPPDoubleContracted(decl, func)
        val declParams = CPP.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
        (decl.decl.contract, declParams.zip(params).toMap)
      case _ =>
        (func.contract, Map.empty)
    }

    val namedO = func.o.where(name = info.name)
    val rewrittenContract = rw.dispatch(contract) // First rewrite contract to register given and yields variables

    val proc =
      cppCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
          {
            val rewrittenParams = rw.variables.collect {
              params.foreach(rw.dispatch)
            }._1
            rw.labelDecls.scope {
              new Procedure[Post](
                returnType = returnType,
                args = rewrittenParams,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.dispatch(func.body)),
                contract = rewrittenContract,
              )(func.blame)(namedO)
            }
          }
        )
      }

    cppFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) =>
        cppFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def rewriteGlobalDecl(decl: CPPGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    for ((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty, skip the declaration: the definition is used instead.
      val info = CPP.getDeclaratorInfo(init.decl)
      var declared = false
      // Some method whose name start with sycl:: do not have to be declared
      // because those methods were just used for resolution and type checking
      if (!info.name.startsWith("sycl::")) {
        val namedO = init.o.where(name = info.name)
        info.params match {
          case Some(params) =>
            cppFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
              new Procedure[Post](
                returnType = t,
                args = rw.variables.collect {
                  params.foreach(rw.dispatch)
                }._1,
                outArgs = Nil,
                typeArgs = Nil,
                body = None,
                contract = rw.dispatch(decl.decl.contract),
                inline = decl.decl.specs.collectFirst { case CPPInline() => () }.nonEmpty,
                pure = decl.decl.specs.collectFirst { case CPPPure() => () }.nonEmpty,
              )(AbstractApplicable)(namedO)
            )
            declared = true
          case None =>
            cppGlobalNameSuccessor(RefCPPGlobalDeclaration(decl, idx)) =
              rw.globalDeclarations.declare(new HeapVariable(t)(namedO))
        }
      }
    }
  }

  def rewriteLocalDecl(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()

    val isArray = decl.decl.specs.collectFirst { case CPPSpecificationType(_: CPPTArray[Pre]) => () }.isDefined
    if(isArray){
      return rewriteArrayDeclaration(decl)
    }

    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    decl.decl.specs.foreach {
      case _: CPPSpecificationType[Pre] =>
      case _ => throw WrongCPPType(decl)
    }

    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head

    val info = CPP.getDeclaratorInfo(init.decl)
    val varO: Origin = init.o.where(name = info.name)
    var v: Variable[Post] = new Variable[Post](t)(varO)

    implicit val o: Origin = init.o
    var result: Statement[Post] = Block(Nil)

    if (init.init.isDefined) {
      val preValue = init.init.get
      (t, preValue) match {
        case (SYCLTEvent(), inv: CPPInvocation[Pre]) if inv.ref.get.name == "sycl::queue::submit" =>
          val (block, syclEventRef) = rewriteSYCLQueueSubmit(inv)
          v = syclEventRef
          result = block
        case (SYCLTEvent(), _) =>
          // Event variable types are changed to the eventClass they are linked to
          val postValue = rw.dispatch(preValue)
          v = new Variable(postValue.t)(varO)
          result = Block(Seq(LocalDecl(v), assignLocal(v.get, postValue)))
        case (SYCLTBuffer(_, _), inv: CPPInvocation[Pre]) if inv.ref.get.isInstanceOf[RefSYCLConstructorDefinition[Pre]] =>
          // Buffer constructor
          if (!t.superTypeOf(rw.dispatch(inv.t))) {
            throw UnexpectedCPPTypeError(t, inv)
          }
          val (block, syclBufferRef) = rewriteSYCLBufferConstruction(inv, Some(varO))
          v = syclBufferRef
          result = block
        case (SYCLTLocalAccessor(_, _), inv: CPPInvocation[Pre]) if inv.ref.get.isInstanceOf[RefSYCLConstructorDefinition[Pre]] =>
          // Local accessor constructor
          if (!t.superTypeOf(rw.dispatch(inv.t))) {
            throw UnexpectedCPPTypeError(t, inv)
          }
          val newValue: NewArray[Post] = rw.dispatch(preValue) match {
            case arr: NewArray[Post] => arr
            case _ => throw Unreachable("A SYCL local accessor should only be able to be initialized with a constructor that returns a new COL array.")
          }
          v = new Variable(newValue.t)(v.o) // new type should be COL array, not local accessor, so update
          syclLocalAccessorVarToDimensions(v) = newValue.dims
          result = Block(Seq(LocalDecl(v), assignLocal(v.get, newValue)))
        case (SYCLTRange(_), _) |  (SYCLTNDRange(_), _) => syclRangeVariableToInitializer(v) = rw.dispatch(preValue)
        case _ => result = Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(preValue))))
      }
    } else {
      t match {
        case _ => result = LocalDecl(v)
      }
    }
    cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
    result
  }

  def local(local: CPPLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case spec: SpecNameTarget[Pre] => rw.specLocal(spec, local, local.blame)
      case _: SpecInvocationTarget[Pre] => throw NotAValue(local)
      case ref: RefCPPParam[Pre] =>
        if (cppCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cppNameSuccessor.ref(RefCPPParam(cppCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cppNameSuccessor.ref(ref))
      case RefCPPFunctionDefinition(_) => throw NotAValue(local)
      case ref@RefCPPGlobalDeclaration(decl, initIdx) =>
        CPP.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None => DerefHeapVariable[Post](cppGlobalNameSuccessor.ref(ref))(local.blame)
          case Some(_) => throw NotAValue(local)
        }
      case ref: RefCPPLocalDeclaration[Pre] => CPP.unwrappedType(local.t) match {
        case _: SYCLTAccessor[Pre] =>
          // Referencing an accessor variable can only be done in kernels, otherwise an error will already have been thrown
          val accessor = syclAccessorSuccessor(ref)
          Deref[Post](currentThis.get, accessor.instanceField.ref)(SYCLAccessorFieldInsufficientReferencePermissionBlame(local))
        case _: SYCLTLocalAccessor[Pre] if currentKernelType.get.isInstanceOf[BasicKernel] => throw SYCLNoLocalAccessorsInBasicKernel(local)
        case _: SYCLTRange[Pre] | _: SYCLTNDRange[Pre] => syclRangeVariableToInitializer(cppNameSuccessor(ref))
        case _ => Local(cppNameSuccessor.ref(ref))
      }
      case RefSYCLAccessMode(decl) => rw.dispatch(decl)
      case RefSYCLConstructorDefinition(_) => throw NotAValue(local)
    }
  }

  def preAssignExpr(preAssign: PreAssignExpression[Pre]): Expr[Post] = {
    CPP.unwrappedType(preAssign.target.t) match {
      case _: SYCLTEvent[Pre] => throw SYCLReassigningOfVariableUnsupported("event", "EventVariable", preAssign)
      case _: SYCLTBuffer[Pre] => throw SYCLReassigningOfVariableUnsupported("buffer", "Buffer", preAssign)
      case SYCLTAccessor(_, _, true) => throw SYCLReassigningOfReadonlyAccessor(preAssign)
      case _ => rw.rewriteDefault(preAssign)
    }
  }

  def invocationStatement(eval: Eval[Pre]): Statement[Post] = {
    val inv = eval.expr.asInstanceOf[CPPInvocation[Pre]]
    inv.ref.get.name match {
      case "sycl::event::wait" if inv.applicable.isInstanceOf[CPPClassMethodOrFieldAccess[Pre]] =>
        implicit val o: Origin = inv.o
        rw.dispatch(inv.applicable.asInstanceOf[CPPClassMethodOrFieldAccess[Pre]].classInstance) match {
          case localVar: Local[Post] =>
            val accessors: Seq[SYCLAccessor[Post]] = currentlyRunningKernels.getOrElse(localVar, throw Unreachable(inv.o.messageInContext("Could not find the event variable in the stored running kernels.")))
            syclKernelTermination(localVar, accessors)
          case _ => throw Unreachable(inv.o.messageInContext("The object on which the wait() method was called is not a locally declared SYCL event."))
        }
      case "sycl::queue::submit" => rewriteSYCLQueueSubmit(inv)._1
      case _ if inv.ref.get.isInstanceOf[RefSYCLConstructorDefinition[Pre]] && inv.t.isInstanceOf[SYCLTBuffer[Pre]] =>
        rewriteSYCLBufferConstruction(inv)._1
      case _ => rw.rewriteDefault(eval)
    }
  }

  def invocation(inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case spec: SpecInvocationTarget[Pre] =>
        rw.specInvocation(None, spec, Nil, args, givenMap, yields, inv, inv.blame)
      case ref: RefCPPFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](cppFunctionSuccessor.ref(ref.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefCPPLambdaDefinition(_) => ???
      case e: RefCPPGlobalDeclaration[Pre] => globalInvocation(e, inv)
      case RefSYCLConstructorDefinition(typ) => syclConstructorInvocation(typ, inv)
    }
  }

  def globalInvocation(e: RefCPPGlobalDeclaration[Pre], inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(applicable, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    val classInstance: Option[Expr[Post]] = applicable match {
      case CPPClassMethodOrFieldAccess(obj, _) => Some(rw.dispatch(obj))
      case _ => None
    }

    e.name match {
      case "sycl::item::get_id" if args.length == 1 => getSimpleWorkItemId(inv, GlobalScope())
      case "sycl::item::get_range" if args.length == 1 => getSimpleWorkItemRange(inv, GlobalScope())
      case "sycl::item::get_linear_id" => getSimpleWorkItemLinearId(inv, GlobalScope())
      case "sycl::nd_item::get_local_id" if args.length == 1 => getSimpleWorkItemId(inv, LocalScope())
      case "sycl::nd_item::get_local_range" if args.length == 1 => getSimpleWorkItemRange(inv, LocalScope())
      case "sycl::nd_item::get_local_linear_id" => getSimpleWorkItemLinearId(inv, LocalScope())
      case "sycl::nd_item::get_group" if args.length == 1 => getSimpleWorkItemId(inv, GroupScope())
      case "sycl::nd_item::get_group_range" if args.length == 1 => getSimpleWorkItemRange(inv, GroupScope())
      case "sycl::nd_item::get_group_linear_id" => getSimpleWorkItemLinearId(inv, GroupScope())
      case "sycl::nd_item::get_global_id" if args.length == 1 => getGlobalWorkItemId(inv)
      case "sycl::nd_item::get_global_range" if args.length == 1 => getGlobalWorkItemRange(inv)
      case "sycl::nd_item::get_global_linear_id" => getGlobalWorkItemLinearId(inv)
      case "sycl::accessor::get_range" => classInstance match {
        case Some(Deref(_, ref)) =>
          val accessor = syclAccessorSuccessor.values.find(acc => ref.decl.equals(acc.instanceField)).get
          LiteralSeq[Post](TCInt(), accessor.rangeIndexFields.map(f => Deref[Post](currentThis.get, f.ref)(SYCLAccessorRangeIndexFieldInsufficientReferencePermissionBlame(inv))))
        case _ => throw NotApplicable(inv)
      }
      case "sycl::local_accessor::get_range" => classInstance match {
        case Some(Local(ref)) =>
          val dimensions = syclLocalAccessorVarToDimensions(ref.decl)
          LiteralSeq[Post](TCInt(), dimensions)
        case _ => throw NotApplicable(inv)
      }
      case "sycl::range::get" => (classInstance, args) match {
        case (Some(seq: LiteralSeq[Post]), Seq(arg)) => SeqSubscript(seq, rw.dispatch(arg))(SYCLRequestedRangeIndexOutOfBoundsBlame(seq, arg)) // Range coming from calling get_range() on a (local)accessor
        case _ => throw NotApplicable(inv)
      }

      case _ =>
        val procedureRef: Ref[Post, Procedure[Post]] = cppFunctionDeclSuccessor.ref((decls, initIdx))
        ProcedureInvocation[Post](
          procedureRef, args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) }
        )(inv.blame)
    }
  }

  private def syclConstructorInvocation(typ: SYCLTConstructableClass[Pre], inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, _, _) = inv
    implicit val o: Origin = inv.o

    typ match {
      case SYCLTRange(_) => SYCLRange[Post](args.map(rw.dispatch))
      case SYCLTNDRange(_) => SYCLNDRange[Post](rw.dispatch(args.head), rw.dispatch(args(1)))
      case SYCLTLocalAccessor(typ, _) =>
        val range = rw.dispatch(args.head).asInstanceOf[SYCLRange[Post]]
        NewArray(rw.dispatch(typ), range.dimensions, 0, false)(SYCLLocalAccessorArraySizeBlame(args.head))
      case _ => ???
    }
  }

  private def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): (Block[Post], Variable[Post]) = {
    // Do not allow given and yields on the invocation
    if (invocation.givenArgs.nonEmpty || invocation.yields.nonEmpty) throw SYCLGivenYieldsOnSYCLMethodsUnsupported(invocation)

    // Get the lambda describing the command group
    val commandGroup = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]]
    val commandGroupBody: Statement[Pre] = commandGroup.body

    // Do not allow contracts for the command group
    if (commandGroup.contract.nonEmpty) throw SYCLContractForCommandGroupUnsupported(commandGroup.contract)

    // Get the kernel declarations in the command group
    val collectedKernelDeclarations: Seq[CPPInvocation[Pre]] = findStatements(commandGroupBody, {
      case Eval(inv: CPPInvocation[Pre]) if inv.ref.isDefined && inv.ref.get.name == "sycl::handler::parallel_for" => Some(inv)
      case _ => None
    })

    // Make sure there is only one kernel declaration in the command group
    if (collectedKernelDeclarations.isEmpty) {
      throw SYCLMissingKernel(commandGroupBody)
    } else if (collectedKernelDeclarations.size > 1) {
      throw SYCLNoMultipleKernels(commandGroupBody, collectedKernelDeclarations(1))
    } else if (collectedKernelDeclarations.head.givenArgs.nonEmpty || collectedKernelDeclarations.head.yields.nonEmpty) {
      // Do not allow given and yields on the kernel
      throw SYCLGivenYieldsOnSYCLMethodsUnsupported(collectedKernelDeclarations.head)
    }

    // Create a class that can be used to create a 'this' object
    // It will be linked to the class made near the end of this method.
    val preEventClass: Class[Pre] = new Class(Nil, Nil, tt)(commandGroup.o)
    this.currentThis = Some(rw.dispatch(ThisObject[Pre](preEventClass.ref)(preEventClass.o)))

    // Generate conditions and accessor objects for each accessor declared in the command group
    val collectedAccessorDeclarations: Seq[CPPLocalDeclaration[Pre]] = findStatements(commandGroupBody, {
      case CPPDeclarationStatement(decl) if CPP.getBaseTypeFromSpecs(decl.decl.specs).isInstanceOf[SYCLTAccessor[Pre]] => Some(decl)
      case _ => None
    })
    val (accessors, accessorRunMethodConditions, accessorParblockConditions, bufferAccessStatements) = rewriteSYCLAccessorDeclarations(collectedAccessorDeclarations)

    // Generate an array each local accessor declared in the command group
    val collectedLocalAccessorDeclarations: Seq[CPPLocalDeclaration[Pre]] = findStatements(commandGroupBody, {
      case CPPDeclarationStatement(decl) if CPP.getBaseTypeFromSpecs(decl.decl.specs).isInstanceOf[SYCLTLocalAccessor[Pre]] => Some(decl)
      case _ => None
    })

    val nrOfCommandGroupStatements = findStatements(commandGroupBody, s => Some(s)).size
    // Check that there is no other code in the command group other than 1 kernel declaration and the found (local) accessors
    if (nrOfCommandGroupStatements > 1 + collectedAccessorDeclarations.size + collectedLocalAccessorDeclarations.size) {
      throw SYCLNoExtraCodeInCommandGroup(commandGroupBody)
    }

    val kernelDimensions = collectedKernelDeclarations.head.args.head
    val kernelDeclaration = collectedKernelDeclarations.head.args(1).asInstanceOf[CPPLambdaDefinition[Pre]]

    // Get the kernel range type (which also checks if it is valid)
    val rangeType = getKernelRangeType(kernelDimensions.t, kernelDeclaration.declarator)

    // Create a block of code for the kernel body based on what type of kernel it is
    val (kernelParBlock, contractRequires, contractEnsures) = rangeType match {
      case SYCLTRange(_) => createBasicKernelBody(kernelDimensions, kernelDeclaration, accessorParblockConditions)
      case SYCLTNDRange(_) => createNDRangeKernelBody(kernelDimensions, kernelDeclaration, accessorParblockConditions, collectedLocalAccessorDeclarations)
      case _ => throw Unreachable("Wrong type for the dimensions parameter of the kernel. " +
        "The dimensions parameter in a kernel declaration is supposed to be of type sycl::range<int> or sycl::nd_range<int>.")
    }

    // Create the pre- and postconditions for the run-method that will hold the generated kernel code
    val kernelRunnerPreCondition = {
      implicit val o: Origin = kernelDeclaration.contract.requires.o
      UnitAccountedPredicate(foldStar(
        currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++
        accessorRunMethodConditions :+
        getKernelQuantifiedCondition(kernelParBlock, removeKernelClassInstancePermissions(contractRequires))
      )(commandGroupBody.o))
    }
    val kernelRunnerPostCondition = {
      implicit val o: Origin = kernelDeclaration.contract.ensures.o
      UnitAccountedPredicate(foldStar(
        currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++
          accessorRunMethodConditions :+
          getKernelQuantifiedCondition(kernelParBlock, removeKernelClassInstancePermissions(contractEnsures))
      )(commandGroupBody.o))
    }

    // Declare the newly generated kernel code inside a run-method
    val kernelRunnerContract = ApplicableContract[Post](kernelRunnerPreCondition, kernelRunnerPostCondition, tt, Nil, Nil, Nil, None)(SYCLKernelRunMethodContractUnsatisfiableBlame(kernelRunnerPreCondition))(commandGroup.o)
    val kernelRunner = new RunMethod[Post](
      body = Some(ParStatement[Post](kernelParBlock)(kernelDeclaration.body.o)),
      contract = kernelRunnerContract,
    )(KernelLambdaRunMethodBlame(kernelDeclaration))(commandGroup.o)

    // Create the surrounding class
    val postEventClass = new Class[Post](
      declarations = currentKernelType.get.getRangeFields ++ accessors.flatMap(acc => acc.instanceField +: acc.rangeIndexFields) ++ Seq(kernelRunner),
      supports = Seq(),
      intrinsicLockInvariant = tt
    )(commandGroup.o.where(name = "SYCL_EVENT_CLASS"))
    rw.globalDeclarations.succeed(preEventClass, postEventClass)

    // Create a variable to refer to the class instance
    val eventClassRef = new Variable[Post](TClass(postEventClass.ref))(commandGroup.o.where(name = "sycl_event_ref"))
    // Store the class ref and read-write accessors to be used when the kernel is done running
    currentlyRunningKernels.put(eventClassRef.get(commandGroup.o), accessors.filter(acc => acc.accessMode.isInstanceOf[SYCLReadWriteAccess[Post]]))

    // Declare a constructor for the class as a separate global method
    val eventClassConstructor = createEventClassConstructor(accessors, preEventClass, commandGroup.o)

    // Create a new class instance and assign it to the class instance variable, then fork that variable
    val result = (Block[Post](
      bufferAccessStatements ++
      Seq(
        LocalDecl[Post](eventClassRef)(commandGroup.o),
        assignLocal(
          eventClassRef.get(commandGroup.o),
          ProcedureInvocation[Post]( // Call constructor
            ref = eventClassConstructor.ref,
            args = currentKernelType.get.getRangeValues ++ accessors.flatMap(acc => acc.buffer.generatedVar.get(acc.buffer.generatedVar.o) +: acc.buffer.range.dimensions.map(dim => dim)),
            Nil, Nil, Nil, Nil,
          )(SYCLKernelRangeInvalidBlame())(commandGroup.o)
        )(commandGroup.o),
        Fork(eventClassRef.get(eventClassRef.o))(SYCLKernelForkBlame(kernelDeclaration))(invocation.o)
      )
    )(invocation.o), eventClassRef)

    // Reset the global variables as we are done processing the kernel
    currentKernelType = None
    currentDimensionIterVars.clear()
    syclAccessorSuccessor.clear()
    currentThis = None

    result
  }

  private def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre], accessorParblockConditions: Seq[Expr[Post]]): (ParBlock[Post], Expr[Post], Expr[Post]) = {
    // Register the kernel dimensions
    val range: Seq[Expr[Post]] = rw.dispatch(kernelDimensions) match {
      case SYCLRange(dims) => dims
      case _ => throw Unreachable("The dimensions parameter of the kernel was not rewritten to a range.")
    }

    currentDimensionIterVars.clear()
    currentDimensionIterVars(GlobalScope()) = mutable.Buffer.empty
    val rangeFields: mutable.Buffer[InstanceField[Post]] = mutable.Buffer.empty
    range.indices.foreach(index => {
      implicit val o: Origin = range(index).o.where(name = s"range$index")
      val instanceField = new InstanceField[Post](TCInt(), Set())
      rangeFields.append(instanceField)
      val iterVar = createRangeIterVar(GlobalScope(), index, Deref[Post](currentThis.get, instanceField.ref)(new SYCLRangeDerefBlame(instanceField)))
      currentDimensionIterVars(GlobalScope()).append(iterVar)
    })
    currentKernelType = Some(BasicKernel(kernelDimensions.o, rangeFields.toSeq, range))

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)
    val idLimits = currentDimensionIterVars(GlobalScope()).map(iterVar => {
      implicit val o: Origin = iterVar.o
      Local[Post](iterVar.variable.ref) >= c_const(0) && Local[Post](iterVar.variable.ref) < iterVar.to
    })
    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the kernels
    val parBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_BASIC_KERNEL")),
      iters = currentDimensionIterVars(GlobalScope()).toSeq,
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ idLimits ++ accessorParblockConditions :+ contractRequires),
      ensures = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ idLimits ++ accessorParblockConditions :+ contractEnsures),
      content = rw.dispatch(kernelDeclaration.body),
    )(SYCLKernelParBlockFailureBlame(kernelDeclaration))

    (parBlock, contractRequires, contractEnsures)
  }

  private def createNDRangeKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre], accessorParblockConditions: Seq[Expr[Post]], localAccessorDeclarations: Seq[CPPLocalDeclaration[Pre]]): (ParBlock[Post], Expr[Post], Expr[Post]) = {
    // Register the kernel dimensions
    val (globalRange, localRange): (Seq[Expr[Post]], Seq[Expr[Post]]) = rw.dispatch(kernelDimensions) match {
      case SYCLNDRange(globalSize: SYCLRange[Post], localRange: SYCLRange[Post]) => (globalSize.dimensions, localRange.dimensions)
      case _ => throw Unreachable("The dimensions parameter of the kernel was not rewritten to an nd_range.")
    }

    currentDimensionIterVars.clear()
    currentDimensionIterVars(LocalScope()) = mutable.Buffer.empty
    currentDimensionIterVars(GroupScope()) = mutable.Buffer.empty
    val rangeFields: mutable.Buffer[InstanceField[Post]] = mutable.Buffer.empty
    localRange.indices.foreach(index => {
      {
        implicit val o: Origin = kernelDimensions.o.where(name = s"group_range$index")
        val groupInstanceField = new InstanceField[Post](TCInt(), Set())
        rangeFields.append(groupInstanceField)
        val groupIterVar = createRangeIterVar(GroupScope(), index, Deref[Post](currentThis.get, groupInstanceField.ref)(new SYCLRangeDerefBlame(groupInstanceField)))
        currentDimensionIterVars(GroupScope()).append(groupIterVar)
      }
      {
        implicit val o: Origin = localRange(index).o.where(name = s"local_range$index")
        val localInstanceField = new InstanceField[Post](TCInt(), Set())
        rangeFields.append(localInstanceField)
        val localIterVar = createRangeIterVar(LocalScope(), index, Deref[Post](currentThis.get, localInstanceField.ref)(new SYCLRangeDerefBlame(localInstanceField)))
        currentDimensionIterVars(LocalScope()).append(localIterVar)
      }
    })
    currentKernelType = Some(NDRangeKernel(kernelDimensions.o, rangeFields.toSeq, globalRange.zip(localRange).flatMap(tuple => Seq(tuple._1, tuple._2))))

    // Add the local accessors
    val localAccessorDecls: mutable.Buffer[Statement[Post]] = mutable.Buffer.empty
    val localAccessorVariables: mutable.Buffer[Variable[Post]] = mutable.Buffer.empty
    localAccessorDeclarations.foreach(localAccDecl => {
      localAccessorDecls.append(rewriteLocalDecl(localAccDecl))
      localAccessorVariables.append(cppNameSuccessor(RefCPPLocalDeclaration(localAccDecl, 0)))
    })

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)
    val localIdLimits = currentDimensionIterVars(LocalScope()).map(iterVar => {
      implicit val o: Origin = iterVar.o
      Local[Post](iterVar.variable.ref) >= c_const(0) && Local[Post](iterVar.variable.ref) < iterVar.to
    })
    val groupIdLimits = currentDimensionIterVars(GroupScope()).map(iterVar => {
      implicit val o: Origin = iterVar.o
      Local[Post](iterVar.variable.ref) >= c_const(0) && Local[Post](iterVar.variable.ref) < iterVar.to
    })

    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the work-items inside work-groups
    val workItemParBlock = ParStatement[Post](ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_ND_RANGE_KERNEL_WORKITEMS")),
      iters = currentDimensionIterVars(LocalScope()).toSeq,
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ groupIdLimits ++ localIdLimits ++ accessorParblockConditions :+ contractRequires),
      ensures = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ groupIdLimits ++ localIdLimits ++ accessorParblockConditions :+ contractEnsures),
      content = rw.dispatch(kernelDeclaration.body)
    )(SYCLKernelParBlockFailureBlame(kernelDeclaration)))

    val quantifiedContractRequires = getKernelQuantifiedCondition(workItemParBlock.impl.asInstanceOf[ParBlock[Post]], removeLocalAccessorConditions(contractRequires, localAccessorVariables.toSeq))
    val quantifiedContractEnsures = getKernelQuantifiedCondition(workItemParBlock.impl.asInstanceOf[ParBlock[Post]], removeLocalAccessorConditions(contractEnsures, localAccessorVariables.toSeq))

    // Create the parblock representing the work-groups
    val workGroupParBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_ND_RANGE_KERNEL_WORKGROUPS")),
      iters = currentDimensionIterVars(GroupScope()).toSeq,
      context_everywhere = tt,
      requires = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ groupIdLimits ++ accessorParblockConditions :+ quantifiedContractRequires),
      ensures = foldStar(currentKernelType.get.getRangeFieldPermissions(currentThis.get) ++ groupIdLimits ++ accessorParblockConditions :+ quantifiedContractEnsures),
      content = Scope(Nil, Block(localAccessorDecls.toSeq :+ workItemParBlock))
    )(SYCLKernelParBlockFailureBlame(kernelDeclaration))

    (workGroupParBlock, quantifiedContractRequires, quantifiedContractEnsures)
  }

  private def rewriteSYCLAccessorDeclarations(decls: Seq[CPPLocalDeclaration[Pre]]): (Seq[SYCLAccessor[Post]], Seq[Expr[Post]], Seq[Expr[Post]], Seq[Statement[Post]]) = {
    // SYCLBuffer to (accessor object, kernelRunnerConditions, kernelParblockConditions, bufferAccessStatements)
    val buffersToAccessorResults: mutable.Map[SYCLBuffer[Post], (SYCLAccessor[Post], Expr[Post], Expr[Post], Statement[Post])] = mutable.Map.empty
    decls.foreach(decl => {
      val accDecl = decl.decl
      if (accDecl.inits.nonEmpty && accDecl.inits.head.init.isDefined && accDecl.inits.head.init.get.isInstanceOf[CPPInvocation[Pre]]) {
        accDecl.inits.head.init.get match {
          case inv@CPPInvocation(_, Seq(bufferRef: CPPLocal[Pre], _, accessModeRef: CPPLocal[Pre]), _, _) =>
            rw.dispatch(accessModeRef)  match {
              case accessMode: SYCLAccessMode[Post] =>
                val buffer = getFromAll(syclBufferSuccessor, cppNameSuccessor(bufferRef.ref.get)).getOrElse(throw SYCLBufferOutOfScopeError(decl))
                val accName = buffer.generatedVar.o.getPreferredNameOrElse().ucamel
                val accO: Origin = SYCLGeneratedAccessorPermissionsOrigin(decl).where(name = accName)
                val dimO: Origin = SYCLGeneratedAccessorPermissionsOrigin(decl)
                if (!CPP.getBaseTypeFromSpecs(accDecl.specs).asInstanceOf[SYCLTAccessor[Pre]].superTypeOf(CPP.unwrappedType(inv.t))) {
                  throw UnexpectedCPPTypeError(CPP.getBaseTypeFromSpecs(accDecl.specs), inv)
                }

                buffersToAccessorResults.get(buffer) match {
                  case Some((foundAcc, _, _, _)) =>
                    // Buffer already exists, so skip creation of fields and use already existing fields
                    val newAcc = SYCLAccessor[Post](buffer, accessMode, foundAcc.instanceField, foundAcc.rangeIndexFields)(accDecl.o)
                    syclAccessorSuccessor(RefCPPLocalDeclaration(decl, 0)) = newAcc

                    // Upgrade read access to write access if the new accessor wants write access, as is done in the SYCL specification in Table 4 of section 3.8.1 (page 27)
                    if (foundAcc.accessMode.isInstanceOf[SYCLReadOnlyAccess[Post]] && accessMode.isInstanceOf[SYCLReadWriteAccess[Post]]) {
                      val (basicPermissions, fieldArrayElementsPermission) = getBasicAccessorPermissions(newAcc, this.currentThis.get)
                      buffersToAccessorResults(buffer) = (newAcc, Star(basicPermissions, fieldArrayElementsPermission)(accO), basicPermissions, getKernelsWaitingForBuffer(buffer, accessMode, decl))
                    }
                  case None =>
                    // No accessor for buffer exist in the command group, so make fields and permissions
                    val instanceField = new InstanceField[Post](buffer.generatedVar.t, Set())(accO)
                    val rangeIndexFields = Seq.range(0, buffer.range.dimensions.size).map(i => new InstanceField[Post](TCInt(), Set())(dimO.where(name = s"${accName}_r$i")))
                    val newAcc = SYCLAccessor[Post](buffer, accessMode, instanceField, rangeIndexFields)(accDecl.o)
                    syclAccessorSuccessor(RefCPPLocalDeclaration(decl, 0)) = newAcc

                    val (basicPermissions, fieldArrayElementsPermission) = getBasicAccessorPermissions(newAcc, this.currentThis.get)
                    buffersToAccessorResults(buffer) = (newAcc, Star(basicPermissions, fieldArrayElementsPermission)(accO), basicPermissions, getKernelsWaitingForBuffer(buffer, accessMode, decl))
                }
              case _ => throw Unreachable(accessModeRef.o.messageInContext("Access mode was rewritten to something invalid."))
            }
          case _ => throw Unreachable(accDecl.inits.head.init.get.o.messageInContext("Accessor declaration is malformed."))
        }
      } else {
        throw Unreachable(decl.o.messageInContext("Accessor declaration is malformed."))
      }
    })

    buffersToAccessorResults.values.foldLeft((Nil, Nil, Nil, Nil): (Seq[SYCLAccessor[Post]], Seq[Expr[Post]], Seq[Expr[Post]], Seq[Statement[Post]]))((result, value) => {
      (result._1 :+ value._1, result._2 :+ value._2, result._3 :+ value._3, result._4 :+ value._4)
    })
  }

  private def getBasicAccessorPermissions(acc: SYCLAccessor[Post], classObj: Expr[Post]): (Expr[Post], Perm[Post]) = {
    val perm: Expr[Post] = acc.accessMode match {
      case SYCLReadWriteAccess() => WritePerm[Post]()(acc.accessMode.o)
      case SYCLReadOnlyAccess() => ReadPerm[Post]()(acc.accessMode.o)
    }

    val rangeIndexDerefs: Seq[Expr[Post]] = acc.rangeIndexFields.map(
      f => Deref[Post](classObj, f.ref)(new SYCLAccessorDimensionDerefBlame(f))(f.o)
    )

    (
      foldStar(
        acc.rangeIndexFields.map(f => Perm(FieldLocation[Post](classObj, f.ref)(f.o), ReadPerm[Post]()(f.o))(f.o)) ++
        Seq(
          Perm(FieldLocation[Post](classObj, acc.instanceField.ref)(acc.instanceField.o), ReadPerm()(acc.instanceField.o))(acc.instanceField.o),
          ValidArray(
            Deref[Post](classObj, acc.instanceField.ref)(new SYCLAccessorDerefBlame(acc.instanceField))(acc.instanceField.o),
            rangeIndexDerefs.reduce((e1, e2) => (e1 * e2)(acc.buffer.o)))(acc.instanceField.o),
        )
      )(acc.instanceField.o),
      Perm(
        ArrayLocation(
          Deref[Post](classObj, acc.instanceField.ref)(new SYCLAccessorDerefBlame(acc.instanceField))(acc.instanceField.o),
          Any()(PanicBlame("The accessor field is not null as that was proven in the previous conditions."))(acc.instanceField.o)
        )(PanicBlame("All indices of the accessor array should be accessible"))(acc.instanceField.o),
        perm
      )(acc.accessMode.o)
    )
  }

  private def createEventClassConstructor(accessors: Seq[SYCLAccessor[Post]], preClass: Class[Pre], commandGroupO: Origin): Procedure[Post] = {
    val t = rw.dispatch(TClass[Pre](preClass.ref))
    rw.globalDeclarations.declare(withResult((result: Result[Post]) => {
      val constructorPostConditions: mutable.Buffer[Expr[Post]] = mutable.Buffer.empty
      val constructorArgs: mutable.Buffer[Variable[Post]] = mutable.Buffer.empty

      // Generate expressions that check the bounds of the given range
      constructorArgs.appendAll(currentKernelType.get.getRangeFields.map(f => new Variable[Post](TCInt())(f.o)))
      constructorPostConditions.appendAll(currentKernelType.get.getConstructorPostConditions(result, constructorArgs))
      val preConditions = UnitAccountedPredicate[Post](currentKernelType.get.getRangeSizeChecksForConstructor(constructorArgs))(commandGroupO)

      accessors.foreach(acc => {
        val newConstructorAccessorArg = new Variable[Post](acc.buffer.generatedVar.t)(acc.instanceField.o)
        val newConstructorAccessorDimensionArgs = acc.rangeIndexFields.map(f => new Variable[Post](TCInt())(f.o))

        val (basicPermissions, fieldArrayElementsPermission) = getBasicAccessorPermissions(acc, result)
        constructorPostConditions.append(basicPermissions)
        constructorPostConditions.append(fieldArrayElementsPermission)
        constructorPostConditions.append(foldStar[Post](
          Eq[Post](
            Deref[Post](result, acc.instanceField.ref)(new SYCLAccessorDerefBlame(acc.instanceField))(acc.instanceField.o),
            Local[Post](newConstructorAccessorArg.ref)(newConstructorAccessorArg.o)
          )(newConstructorAccessorArg.o) +:
          Seq.range(0, acc.rangeIndexFields.size).map(i =>
            Eq[Post](
              Deref[Post](result, acc.rangeIndexFields(i).ref)(new SYCLAccessorDimensionDerefBlame(acc.rangeIndexFields(i)))(acc.rangeIndexFields(i).o),
              Local[Post](newConstructorAccessorDimensionArgs(i).ref)(newConstructorAccessorDimensionArgs(i).o)
            )(newConstructorAccessorDimensionArgs(i).o)
          )
        )(acc.o))

        constructorArgs.append(newConstructorAccessorArg)
        constructorArgs.appendAll(newConstructorAccessorDimensionArgs)
      })

      {
        implicit val o: Origin = commandGroupO
        new Procedure[Post](
          returnType = t,
          args = constructorArgs.toSeq,
          outArgs = Seq(), typeArgs = Seq(), body = None,
          contract = ApplicableContract[Post](
            preConditions,
            SplitAccountedPredicate(
              left = UnitAccountedPredicate((result !== Null()) && (TypeOf(result) === TypeValue(t))),
              right = UnitAccountedPredicate[Post](foldStar[Post](constructorPostConditions.toSeq :+ IdleToken[Post](result))),
            ),
            tt, Seq(), Seq(), Seq(), None
          )(SYCLKernelConstructorNontrivialUnsatisfiableBlame(commandGroupO))
        )(SYCLKernelConstructorCallableFailureBlame())(o.where(name = "event_constructor"))
      }
    })(commandGroupO))
  }

  // Returns what kind of kernel we are working with and check that the kernel ranges match with the (nd_)item ranges
  private def getKernelRangeType(complicatedKernelRangeType: Type[Pre], lambdaDeclarator: CPPDeclarator[Pre]): Type[Pre] = {
    val kernelRangeType = complicatedKernelRangeType match {
      case primitive: CPPPrimitiveType[Pre] => CPP.getBaseTypeFromSpecs(primitive.specifiers)
      case typ => typ
    }
    val lambdaParams = CPP.paramsFromDeclarator(lambdaDeclarator)
    if (lambdaParams.size != 1) {
      throw SYCLIncorrectParallelForLambdaArgument(lambdaDeclarator.o)
    }
    val lambdaArgType = CPP.getBaseTypeFromSpecs(lambdaParams.head.specifiers)
    (kernelRangeType, lambdaArgType) match {
      case (SYCLTRange(rangeDimCount), SYCLTItem(itemDimCount)) if rangeDimCount == itemDimCount =>
      case (SYCLTNDRange(rangeDimCount), SYCLTNDItem(itemDimCount)) if rangeDimCount == itemDimCount =>
      case _ => throw SYCLIncorrectParallelForLambdaArgument(lambdaDeclarator.o)
    }
    kernelRangeType
  }

  // Generate the IterVariables that are passed to the parblock as ranges
  private def createRangeIterVar(scope: KernelScopeLevel, dimension: Int, maxRange: Expr[Post])(implicit o: Origin): IterVariable[Post] = {
    val variable = new Variable[Post](TCInt())(o.where(name = s"${scope.idName}_$dimension"))
    new IterVariable[Post](variable, CIntegerValue(0), maxRange)
  }

  // Used for generation the contract for the method wrapping the parblocks
  // Code based on the quantify() method in ParBlockEncoder
  private def getKernelQuantifiedCondition(block: ParBlock[Post], condition: Expr[Post])(implicit o: Origin): Expr[Post] = {
    val parEncoder = new ParBlockEncoder[Pre]()
    val rangesMap: mutable.Map[Variable[Post], (Expr[Post], Expr[Post])] = mutable.Map.empty
    block.iters.map(iterVar => rangesMap.put(iterVar.variable, (iterVar.from, iterVar.to)))

    val conditions = AstBuildHelpers.unfoldStar(condition)
    val vars = block.iters.map(_.variable).toSet

    val rewrittenExpr = conditions.map(cond => {
      val quantVars = parEncoder.depVars(vars, cond)
      val nonQuantVars = vars.diff(quantVars)

      val scale = (x: Expr[Post]) => nonQuantVars.foldLeft(x)((body, iter) => {
        val scale = rangesMap(iter)._2 - rangesMap(iter)._1
        Scale(scale, body)(PanicBlame("Par block was checked to be non-empty"))
      })

      if (quantVars.isEmpty) scale(cond)
      else rw.variables.scope {
        val range = quantVars.map(v =>
          rangesMap(v)._1 <= Local[Post](v.ref) &&
            Local[Post](v.ref) < rangesMap(v)._2
        ).reduceOption[Expr[Post]](And(_, _)).getOrElse(tt)

        cond match {
          case Forall(bindings, Nil, body) =>
            Forall(bindings ++ quantVars, Nil, range ==> scale(body))
          case s@Starall(bindings, Nil, body) =>
            Starall(bindings ++ quantVars, Nil, range ==> scale(body))(s.blame)
          case other =>
            Starall(quantVars.toSeq, Nil, range ==> scale(other))(ParBlockNotInjective(block, other))
        }
      }
    })

    AstBuildHelpers.foldStar(rewrittenExpr)
  }

  private def getSimpleWorkItemId(inv: CPPInvocation[Pre], level: KernelScopeLevel) (implicit o: Origin) : Expr[Post] = {
    SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(level).map(iterVar => iterVar.variable.get).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv))
  }

  private def getSimpleWorkItemRange(inv: CPPInvocation[Pre], level: KernelScopeLevel)(implicit o: Origin): Expr[Post] = {
    SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(level).map(iterVar => iterVar.to).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv))
  }

  private def getSimpleWorkItemLinearId(inv: CPPInvocation[Pre], level: KernelScopeLevel)(implicit o: Origin): Expr[Post] = {
    val args: Seq[Expr[Post]] = (currentDimensionIterVars(level).map(iterVar => iterVar.variable.get) ++ currentDimensionIterVars(level).map(iterVar => iterVar.to)).toSeq

    currentDimensionIterVars(level).size match {
      case i@(2 | 3) => syclHelperFunctions(s"sycl_:_:linearize_$i")(args, SYCLItemMethodInvocationBlame(inv), o)
      case _ => currentDimensionIterVars(level).head.variable.get
    }
  }

  private def getGlobalWorkItemId(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val args: Seq[Expr[Post]] = Seq(
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(LocalScope()).map(iterVar => iterVar.variable.get).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv)),
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(GroupScope()).map(iterVar => iterVar.variable.get).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv)),
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(LocalScope()).map(iterVar => iterVar.to).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv)),
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(GroupScope()).map(iterVar => iterVar.to).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv))
    )

    syclHelperFunctions("sycl_:_:linearize_2")(args, SYCLItemMethodInvocationBlame(inv), o)
  }

  private def getGlobalWorkItemRange(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val args: Seq[Expr[Post]] = Seq(
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(LocalScope()).map(iterVar => iterVar.to).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv)),
      SeqSubscript[Post](LiteralSeq(TCInt(), currentDimensionIterVars(GroupScope()).map(iterVar => iterVar.to).toSeq), rw.dispatch(inv.args.head))(SYCLItemMethodSeqBoundFailureBlame(inv))
    )

    syclHelperFunctions("sycl_:_:nd_item_:_:get_global_range")(args, SYCLItemMethodInvocationBlame(inv), o)
  }

  private def getGlobalWorkItemLinearId(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val ids = currentDimensionIterVars(GroupScope()).indices.map(i => {
      getGlobalWorkItemId(CPPInvocation[Pre](tt, Seq(c_const(i)), Nil, Nil)(PanicBlame(s"Method sycl::nd_item::get_global_id($i) should callable here.")))
    })
    val ranges = currentDimensionIterVars(GroupScope()).indices.map(i => {
      getGlobalWorkItemRange(CPPInvocation[Pre](tt, Seq(c_const(i)), Nil, Nil)(PanicBlame(s"Method sycl::nd_item::get_global_range($i) should callable here.")))
    })

    ids.size match {
      case i@(2 | 3) => syclHelperFunctions(s"sycl_:_:linearize_$i")(ids ++ ranges, SYCLItemMethodInvocationBlame(inv), o)
      case _ => ids.head
    }
  }

  private def rewriteSYCLBufferConstruction(inv: CPPInvocation[Pre], maybeVarNameO: Option[Origin] = None): (Statement[Post], Variable[Post]) = {
    implicit val o: Origin = inv.o
    val varNameO = maybeVarNameO.getOrElse(o)
    val preType = inv.t.asInstanceOf[SYCLTBuffer[Pre]]
    val postType = rw.dispatch(preType).asInstanceOf[SYCLTBuffer[Post]]

    // Generate or get the exclusive access predicate and copy procedures
    val (exclusiveAccessPredicate, copyHostDataToBufferProcedure, _) = syclBufferTypeToHostPredicateAndProcedures.get(postType) match {
      case Some(triple) => triple
      case None =>
        val preGeneratedPredicate = preType.generateExclusiveAccessPredicate()
        val postGeneratedPredicate = postType.generateExclusiveAccessPredicate()
        rw.globalDeclarations.succeed(preGeneratedPredicate, postGeneratedPredicate)
        val preCopyHostDataToBufferProcedure = preType.generateCopyHostDataToBufferProcedure()
        val postCopyHostDataToBufferProcedure = postType.generateCopyHostDataToBufferProcedure()
        rw.globalDeclarations.succeed(preCopyHostDataToBufferProcedure, postCopyHostDataToBufferProcedure)
        val preCopyBufferToHostDataProcedure = preType.generateCopyBufferToHostDataProcedure(preGeneratedPredicate.ref)
        val postCopyBufferToHostDataProcedure = postType.generateCopyBufferToHostDataProcedure(postGeneratedPredicate.ref)
        rw.globalDeclarations.succeed(preCopyBufferToHostDataProcedure, postCopyBufferToHostDataProcedure)

        syclBufferTypeToHostPredicateAndProcedures.put(postType, (postGeneratedPredicate, postCopyHostDataToBufferProcedure, postCopyBufferToHostDataProcedure))
        (postGeneratedPredicate, postCopyHostDataToBufferProcedure, postCopyBufferToHostDataProcedure)
    }

    // Get arguments
    val hostData = rw.dispatch(inv.args.head)
    val range = rw.dispatch(inv.args(1)) match {
      case r: SYCLRange[Post] => r
      case _ => throw Unreachable("The range parameter of the buffer was not rewritten to a range.")
    }

    // Call the method that copies the hostData contents to the buffer
    val args = Seq(hostData, range.size)
    val copyInv = ProcedureInvocation[Post](copyHostDataToBufferProcedure.ref, args, Nil, Nil, Nil, Nil)(SYCLBufferConstructionInvocationBlame(inv))

    // Fold predicate to gain exclusive access to the hostData
    val gainExclusiveAccess = Fold(PredicateApply[Post](exclusiveAccessPredicate.ref, args, WritePerm()))(SYCLBufferConstructionFoldFailedBlame(inv))

    val v = new Variable[Post](TArray(postType.typ))(varNameO)
    syclBufferSuccessor.top.put(v, SYCLBuffer[Post](hostData, v, range, postType))

    val result: Statement[Post] = Block(Seq(LocalDecl(v), assignLocal(v.get, copyInv), gainExclusiveAccess))
    (result, v)
  }

  private def destroySYCLBuffer(buffer: SYCLBuffer[Post], scope: CPPLifetimeScope[_]): Statement[Post] = {
    implicit val o: Origin = buffer.o

    // Wait for SYCL kernels that access the buffer to finish executing
    val kernelsToTerminate = currentlyRunningKernels.filter(tuple => tuple._2.exists(acc => acc.buffer.equals(buffer)))
    val kernelTerminations = kernelsToTerminate.map(tuple => syclKernelTermination(tuple._1, tuple._2)).toSeq

    // Get the exclusive access predicate and copy procedures
    val (exclusiveAccessPredicate, _, copyBufferToHostDataProcedure) = syclBufferTypeToHostPredicateAndProcedures(buffer.typ)

    // Call the method that copies the buffer contents to the hostData
    val copyInv = ProcedureInvocation[Post](copyBufferToHostDataProcedure.ref, Seq(buffer.hostData, buffer.generatedVar.get), Nil, Nil, Nil, Nil)(SYCLBufferDestructionInvocationBlame(buffer.generatedVar, scope))

    // Unfold predicate to release exclusive access to the hostData
    val args = Seq(buffer.hostData, buffer.range.size)
    val removeExclusiveAccess = Unfold(PredicateApply[Post](exclusiveAccessPredicate.ref, args, WritePerm()))(SYCLBufferDestructionUnfoldFailedBlame(buffer.generatedVar, scope))

    Block(kernelTerminations ++ Seq(Eval(copyInv), removeExclusiveAccess))
  }

  private def getKernelsWaitingForBuffer(buffer: SYCLBuffer[Post], mode: SYCLAccessMode[Post], source: CPPLocalDeclaration[Pre]): Statement[Post] = {
    implicit val o: Origin = source.o

    val kernelsToWaitFor: mutable.Map[Local[Post], Seq[SYCLAccessor[Post]]] = mode match {
      case SYCLReadOnlyAccess() => currentlyRunningKernels.filter(tuple => tuple._2.exists(acc => acc.accessMode.isInstanceOf[SYCLReadWriteAccess[Post]] && acc.buffer.equals(buffer)))
      case SYCLReadWriteAccess() => currentlyRunningKernels.filter(tuple => tuple._2.exists(acc => acc.buffer.equals(buffer)))
    }

    Block[Post](kernelsToWaitFor.map(tuple => syclKernelTermination(tuple._1, tuple._2)).toSeq)
  }

  def rewriteSubscript(sub: AmbiguousSubscript[Pre]): Expr[Post] = sub match {
    case AmbiguousSubscript(base: CPPLocal[Pre], index) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 1, _) => ArraySubscript[Post](
          Deref[Post](
            currentThis.get,
            syclAccessorSuccessor(base.ref.get).instanceField.ref
          )(new SYCLAccessorDerefBlame(syclAccessorSuccessor(base.ref.get).instanceField))(sub.o),
          rw.dispatch(index)
        )(SYCLAccessorArraySubscriptErrorBlame(sub))(sub.o)
        case t: SYCLTAccessor[Pre] => throw SYCLWrongNumberOfSubscriptsForAccessor(sub, 1, t.dimCount)
        case _ => ???
      }
    case AmbiguousSubscript(AmbiguousSubscript(base: CPPLocal[Pre], indexX), indexY) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      implicit val o: Origin = sub.o
      val accessor = syclAccessorSuccessor(base.ref.get)
      val linearizeArgs = Seq(rw.dispatch(indexX), rw.dispatch(indexY),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(0).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(0))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(1).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(1)))
      )
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 2, _) => ArraySubscript[Post](
          Deref[Post](currentThis.get, accessor.instanceField.ref)(new SYCLAccessorDerefBlame(accessor.instanceField)),
          syclHelperFunctions("sycl_:_:linearize_2")(linearizeArgs, SYCLAccessorArraySubscriptLinearizeInvocationBlame(sub, base, Seq(indexX, indexY)), o)
        )(SYCLAccessorArraySubscriptErrorBlame(sub))
        case t: SYCLTAccessor[Pre] => throw SYCLWrongNumberOfSubscriptsForAccessor(sub, 2, t.dimCount)
        case _ => ???
      }
    case AmbiguousSubscript(AmbiguousSubscript(AmbiguousSubscript(base: CPPLocal[Pre], indexX), indexY), indexZ) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      implicit val o: Origin = sub.o
      val accessor = syclAccessorSuccessor(base.ref.get)
      val linearizeArgs = Seq(rw.dispatch(indexX), rw.dispatch(indexY), rw.dispatch(indexZ),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(0).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(0))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(1).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(1))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(2).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(2)))
      )
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 3, _) => ArraySubscript[Post](
          Deref[Post](currentThis.get, accessor.instanceField.ref)(new SYCLAccessorDerefBlame(accessor.instanceField)),
          syclHelperFunctions("sycl_:_:linearize_3")(linearizeArgs, SYCLAccessorArraySubscriptLinearizeInvocationBlame(sub, base, Seq(indexX, indexY, indexZ)), o)
        )(SYCLAccessorArraySubscriptErrorBlame(sub))
        case t: SYCLTAccessor[Pre] => throw SYCLWrongNumberOfSubscriptsForAccessor(sub, 3, t.dimCount)
        case _ => ???
      }
    case _ => rw.rewriteDefault(sub)
  }

  def rewriteLifetimeScope(scope: CPPLifetimeScope[Pre]): Statement[Post] = {
    implicit val o: Origin = scope.o

    syclBufferSuccessor.push(mutable.Map.empty)

    val rewrittenBody = rw.dispatch(scope.body)

    // Destroy all buffers and copy their data back to host
    val bufferDestructions: Seq[Statement[Post]] = syclBufferSuccessor.pop().map(tuple => destroySYCLBuffer(tuple._2, scope)).toSeq

    Block[Post](rewrittenBody +: bufferDestructions)
  }

  def deref(deref: CPPClassMethodOrFieldAccess[Pre]): Expr[Post] = {
    deref.ref.get match {
      case spec: SpecDerefTarget[Pre] => rw.specDeref(deref.classInstance, spec, deref, deref.blame)
      case target: RefCPPGlobalDeclaration[Pre] => ???
      case target: SpecInvocationTarget[Pre] => ???
    }
  }

  private def removeKernelClassInstancePermissions(e: Expr[Post]): Expr[Post] = {
    implicit val o = e.o
    e match {
      case _ if e.t != TResource[Post]() => e
      case s@Starall(bindings, triggers, body) => Starall(bindings, triggers, removeKernelClassInstancePermissions(body))(s.blame)
      case ModelAbstractState(model, state) => ModelAbstractState(removeKernelClassInstancePermissions(model), removeKernelClassInstancePermissions(state))
      case ModelState(model, perm, state) => ModelState(removeKernelClassInstancePermissions(model), removeKernelClassInstancePermissions(perm), removeKernelClassInstancePermissions(state))
      case s@Scale(expr, res) => Scale(removeKernelClassInstancePermissions(expr), removeKernelClassInstancePermissions(res))(s.blame)
      case Star(left, right) => Star(removeKernelClassInstancePermissions(left), removeKernelClassInstancePermissions(right))
      case Wand(left, right) => Wand(removeKernelClassInstancePermissions(left), removeKernelClassInstancePermissions(right))
      case Implies(left, right) => Implies(removeKernelClassInstancePermissions(left), removeKernelClassInstancePermissions(right))

      case ActionPerm(Deref(obj, _), _) if obj.equals(this.currentThis.get) => tt
      case ModelPerm(Deref(obj, _), _) if obj.equals(this.currentThis.get) => tt
      case Perm(FieldLocation(obj, _), _) if obj.equals(this.currentThis.get) => tt
      case PointsTo(FieldLocation(obj, _), _, _) if obj.equals(this.currentThis.get) => tt
      case Value(FieldLocation(obj, _)) if obj.equals(this.currentThis.get) => tt
      case Perm(AmbiguousLocation(ArraySubscript(Deref(obj, _), _)), _) if obj.equals(this.currentThis.get) => tt
      case PointsTo(AmbiguousLocation(ArraySubscript(Deref(obj, _), _)), _, _) if obj.equals(this.currentThis.get) => tt
      case Value(AmbiguousLocation(ArraySubscript(Deref(obj, _), _))) if obj.equals(this.currentThis.get) => tt
      case e => e
    }
  }

  private def removeLocalAccessorConditions(conditions: Expr[Post], localAccessorDecls: Seq[Variable[Post]]): Expr[Post] =
    foldStar(unfoldStar(conditions).filterNot(expr => expr.exists({
      case l: Local[Post] if localAccessorDecls.contains(l.ref.decl) => true
    })))(conditions.o)

  private def syclKernelTermination(variable: Local[Post], accessors: Seq[SYCLAccessor[Post]])(implicit o: Origin): Statement[Post] = {
    currentlyRunningKernels.remove(variable)
    Block(
      Join(variable)(SYCLKernelJoinBlame()) +:
      accessors.collect({
        case SYCLAccessor(buffer, SYCLReadWriteAccess(), instanceField, _) =>
          assignLocal(
            buffer.generatedVar.get,
            Deref[Post](variable, instanceField.ref)(new SYCLAccessorDerefBlame(instanceField))
          )
      })
    )
  }

  private def findStatements[S](stat: Statement[Pre], pred: Statement[Pre] => Option[S]): Seq[S] = stat match {
    case Scope(_, body) => findStatements(body, pred)
    case CPPLifetimeScope(body) => findStatements(body, pred)
    case Block(statements) => statements.flatMap(s => findStatements(s, pred))
    case s => pred(s).toSeq
  }

  def assignliteralArray(array: Variable[Post], exprs: Seq[Expr[Pre]], origin: Origin): Seq[Statement[Post]] = {
    implicit val o: Origin = origin
    (exprs.zipWithIndex.map {
      case (value, index) => Assign[Post](AmbiguousSubscript(array.get, c_const(index))(PanicBlame("The explicit initialization of an array in CPP should never generate an assignment that exceeds the bounds of the array")), rw.dispatch(value))(
        PanicBlame("Assignment for an explicit array initializer cannot fail."))
      }
    )
  }

  def rewriteArrayDeclaration(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head
    val info = CPP.getDeclaratorInfo(init.decl)
    implicit val o: Origin = init.o

    decl.decl.specs match {
      case Seq(CPPSpecificationType(cta@CPPTArray(sizeOption, oldT))) =>
        val t = rw.dispatch(oldT)
        val v = new Variable[Post](TPointer(t))(o.sourceName(info.name))
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v

        (sizeOption, init.init) match {
          case (None, None) => throw WrongCPPType(decl)
          case (Some(size), None) =>
            val newArr = NewPointerArray[Post](t, rw.dispatch(size))(cta.blame)
            Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
          case (None, Some(CPPLiteralArray(exprs))) =>
            val newArr = NewPointerArray[Post](t, c_const[Post](exprs.size))(cta.blame)
            Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)) ++ assignliteralArray(v, exprs, o))
          case (Some(size), Some(CPPLiteralArray(exprs))) =>
            val realSize = isConstantInt(size).filter(_ >= 0).getOrElse(throw WrongCPPType(decl))
            if(realSize < exprs.size) logger.warn(s"Excess elements in array initializer: '${decl}'")
            val newArr = NewPointerArray[Post](t, c_const[Post](realSize))(cta.blame)
            Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)) ++ assignliteralArray(v, exprs.take(realSize.intValue), o))
          case _ => throw WrongCPPType(decl)
        }
      case _ => throw WrongCPPType(decl)
    }
  }

  def result(ref: RefCPPFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cppFunctionSuccessor.ref(ref.decl))

  def result(ref: RefCPPGlobalDeclaration[Pre])(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cppFunctionSuccessor.ref(defn.decl))
      case None => Result[Post](cppFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def arrayType(t: CPPTArray[Pre]): Type[Post] = {
    // TODO: we should not use pointer here
    TPointer(rw.dispatch(t.innerType))
  }
}
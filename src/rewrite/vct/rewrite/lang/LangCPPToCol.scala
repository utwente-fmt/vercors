package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.{FuncTools, ScopedStack}
import vct.col.ast.{CPPLocalDeclaration, _}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.NotApplicable
import vct.col.resolve.ctx._
import vct.col.resolve.lang.{CPP, Util}
import vct.col.rewrite.ParBlockEncoder.ParBlockNotInjective
import vct.col.rewrite.{Generation, ParBlockEncoder, Rewritten}
import vct.col.util.AstBuildHelpers.{assignLocal, constOrigin, _}
import vct.col.util.{AstBuildHelpers, SuccessionMap}
import vct.result.Message
import vct.result.VerificationError.{SystemError, Unreachable, UserError}
import vct.rewrite.lang.LangSpecificToCol.NotAValue

import scala.collection.immutable.Seq
import scala.collection.mutable

case object LangCPPToCol {

  private case class WrongCPPType(decl: CPPLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongCPPType"
    override def text: String =
      decl.o.messageInContext(s"This declaration has a type that is not supported.")
  }

  private case class CPPDoubleContracted(decl: CPPGlobalDeclaration[_], defn: CPPFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"
    override def text: String =
      Message.messagesInContext(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      )
  }

  private abstract class CPPInvocationBlame() extends Blame[InvocationFailure] {
    def preconditionFailed(node: InvokingNode[_]): Unit

    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(_, _, node) => preconditionFailed(node)
      case ContextEverywhereFailedInPre(_, _) => PanicBlame("Item methods do not contain context_everywhere clauses, so cannot fail on a context_everywhere clause.").blame(error)
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

  private case class SYCLHeaderItemNotFound(itemType: String, itemName: String) extends SystemError {
    override def text: String = s"Could not find the $itemType $itemName, is the <sycl/sycl.hpp> header included?"
  }

  private case class SYCLReassigningOfVariableUnsupported(objectName: String, codeExtension: String, ass: PreAssignExpression[_]) extends UserError {
    override def code: String = "syclUnsupportedReassigningOf" + codeExtension
    override def text: String = ass.o.messageInContext(s"Reassigning variables holding a SYCL " + objectName + " is not supported.")
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

  private case class SYCLItemMethodInvocationBlame(inv: CPPInvocation[_]) extends CPPInvocationBlame {
    override def preconditionFailed(node: InvokingNode[_]): Unit = inv.blame.blame(SYCLItemMethodPreconditionFailed(node))
  }

  private case class SYCLKernelForkNull(node: Fork[_]) extends UserError {
    override def code: String = "syclKernelForkNull"
    override def text: String = node.o.messageInContext("This event variable might not be linked to a kernel submission to a queue.")
  }

  case object RangeDimensionCheck extends OriginContent
  case object NDRangeDimensionCheck extends OriginContent

  private def RangeDimensionCheckOrigin(iterVarOrRange: Expr[_]) =
    iterVarOrRange.o
      .where(name = "RangeDimensionCheck")
      .withContent(RangeDimensionCheck)

  private def NDRangeDimensionCheckOrigin(range: Expr[_], dimension: Option[Int] = None) =
    range.o
      .where(name = "NDRangeDimensionCheck", context = s"range dimension ${dimension.getOrElse("?")}")
      .withContent(NDRangeDimensionCheck)

  private case class SYCLKernelPreconditionNotEstablished(error: RunnablePreconditionNotEstablished) extends UserError {
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
      case ParPreconditionFailed(_, _) =>
        PanicBlame("Kernel parallel block precondition cannot fail, since an identical predicate is required before.").blame(error)
      case ParBlockPostconditionFailed(failure, _) =>
        kernel.blame.blame(SYCLKernelLambdaFailure(KernelPostconditionFailed(failure, Right(kernel))))
      case ParBlockMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
    }
  }

  private case class SYCLKernelRangeInvalidBlame() extends Blame[AssertFailed] {
    private case class KernelRangeInvalidError(error: AssertFailed) extends UserError {
      override def code: String = "syclKernelRangeInvalid"

      override def text: String = error.failure.node.o match {
        case o if o.find[RangeDimensionCheck.type].isDefined => o.messageInContext("All range dimensions should be greater or equal to zero.")
        case o if o.find[NDRangeDimensionCheck.type].isDefined => o.messageInContext(
          "Every global range dimension should be divisible by the local range dimension at the same index," +
            " and the local range dimension should be greater than 0 to avoid division by zero. " +
            "All global range dimensions should be greater or equal to zero.")
        case _ => Message.messagesInContext((error.node.o, "Assertion may not hold, since ..."), (error.failure.node.o, "... " + error.failure.descCompletion))
      }
    }

    override def blame(error: AssertFailed): Unit = throw KernelRangeInvalidError(error)
  }

  private case class SYCLBufferConstructionFailed(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "syclBufferConstructionFailed"

    override def text: String = inv.o.messageInContext(
      "This buffer cannot be constructed because there is insufficient permission to copy and exclusively claim the hostData. " +
      "Write permission to the hostData over the entire bufferRange is required."
    )
  }

  private case class SYCLBufferConstructionInvocationBlame(inv: CPPInvocation[_]) extends CPPInvocationBlame {
    override def preconditionFailed(node: InvokingNode[_]): Unit = throw SYCLBufferConstructionFailed(inv)
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

  private case class SYCLBufferDestructionInvocationBlame(bufferDecl: Variable[_], scope: CPPLifetimeScope[_]) extends CPPInvocationBlame {
    override def preconditionFailed(node: InvokingNode[_]): Unit = throw SYCLBufferDestructionFailed(bufferDecl, scope)
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
      case TerminationMeasureFailed(_, _, _) => PanicBlame("This kernel class constructor should aleays be able to terminate.").blame(error)
      case SignalsFailed(_, _) | ExceptionNotInSignals(_) => PanicBlame("This kernel class constructor contains no signals clause.").blame(error)
    }
  }

  private class SYCLAccessorDerefBlame(accField: InstanceField[_]) extends PanicBlame(accField.o.messageInContext("There should always be enough permission to dereference the accessor field."))

  private class SYCLAccessorDimensionDerefBlame(accDimField: InstanceField[_]) extends PanicBlame(accDimField.o.messageInContext("There should always be enough permission to dereference the range dimension field of an accessor."))

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

  private case class SYCLBufferLockBlame(source: CPPLocalDeclaration[_]) extends Blame[AssertFailed] {
    private case class SYCLBufferLockError(error: AssertFailed) extends UserError {
      override def code: String = "syclBufferLock"
      override def text: String = source.o.messageInContext("Insufficient permission to the buffer.")
    }

    override def blame(error: AssertFailed): Unit = throw SYCLBufferLockError(error)
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

  case object SYCLGeneratedAccessorPermissions extends OriginContent

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

  val savedGlobalDeclarations: mutable.Map[String, mutable.Buffer[(CPPGlobalDeclaration[Pre], Boolean)]] = mutable.Map.empty
  val savedPredicates: mutable.Buffer[Predicate[Pre]] = mutable.Buffer.empty

  val syclBufferSuccessor: ScopedStack[mutable.Map[Variable[Post], SYCLBuffer[Post]]] = ScopedStack()
  val currentRunningKernels: mutable.Map[Local[Post], Seq[SYCLAccessor[Post]]] = mutable.Map.empty
  val currentAccessorSubstitutions: mutable.Map[CPPNameTarget[Pre], SYCLAccessor[Post]] = mutable.Map.empty
  val currentReplacementsForAccessors: mutable.Map[Expr[Post], SYCLAccessor[Post]] = mutable.Map.empty

  val currentDimensions: mutable.Map[KernelScopeLevel, Seq[IterVariable[Post]]] = mutable.Map.empty
  var currentKernelType: Option[KernelType] = None
  var currentThis: Option[Expr[Post]] = None

  sealed abstract class KernelScopeLevel(val idName: String)
  case class GlobalScope() extends KernelScopeLevel("GLOBAL_ID")
  case class LocalScope() extends KernelScopeLevel("LOCAL_ID")
  case class GroupScope() extends KernelScopeLevel("GROUP_ID")

  sealed abstract class KernelType()
  case class BasicKernel(globalRangeSizes: Seq[Expr[Post]]) extends KernelType()
  case class NDRangeKernel(globalRangeSizes: Seq[Expr[Post]], localRangeSizes: Seq[Expr[Post]]) extends KernelType()

  case class SYCLBuffer[Post](hostData: Expr[Post], generatedVar: Variable[Post], range: SYCLRange[Post], bufferLockVar: Variable[Post])(implicit val o: Origin)
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

  def storePredicate(pred: Predicate[Pre]): Unit = {
    if (pred.o.find[SourceName].contains(SourceName("sycl::buffer::exclusive_hostData_access"))) {
      savedPredicates.append(pred)
    }
  }

  def checkPredicateFoldingAllowed(predRes: Expr[Pre]): Unit = predRes match {
    case CPPInvocation(CPPLocal("sycl::buffer::exclusive_hostData_access", Seq()), _, _, _)  => throw SYCLPredicateFoldingNotAllowed(predRes)
    case _ =>
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

  def rewriteGlobalDecl(decl: CPPGlobalDeclaration[Pre], mustDeclare: Boolean = false): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    for ((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty, skip the declaration: the definition is used instead.
      val info = CPP.getDeclaratorInfo(init.decl)
      var declared = false
      // Some method whose name start with sycl:: do not have to be declared
      // because those methods were just used for resolution and type checking
      if (mustDeclare || !info.name.startsWith("sycl::") || info.name.startsWith("sycl::item") || info.name.startsWith("sycl::nd_item") || info.name.startsWith("sycl::accessor::linearize")) {
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
      if(info.name.startsWith("sycl::")) {
        // Add all sycl methods to a map, which states which methods have not been declared yet.
        // We do this because some methods we only want to declare when they are actually called,
        // as proving the entire SYCL header file impacts verification speed
        if (!savedGlobalDeclarations.contains(info.name)) {
          savedGlobalDeclarations.put(info.name, mutable.Buffer.empty)
        }
        savedGlobalDeclarations(info.name).append((decl, declared))
      }
    }
  }

  def rewriteLocalDecl(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    decl.decl.specs.foreach {
      case _: CPPSpecificationType[Pre] =>
      case _ => throw WrongCPPType(decl)
    }

    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head

    val info = CPP.getDeclaratorInfo(init.decl)
    val varO: Origin = init.o.where(name = info.name)
    t match {
      case cta @ CPPTArray(Some(size), t) =>
        if (init.init.isDefined) throw WrongCPPType(decl)
        implicit val o: Origin = init.o
        val v = new Variable[Post](TArray(t))(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        val newArr = NewArray[Post](t, Seq(size), 0)(cta.blame)
        Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
      case typ =>
        implicit val o: Origin = init.o
        var v = new Variable[Post](t)(varO)
        var result: Statement[Post] = LocalDecl(v)
        if (init.init.isDefined) {
          val preValue = init.init.get
          // Update the current event map if needed
          preValue match {
            case inv: CPPInvocation[Pre] if inv.ref.get.name == "sycl::queue::submit" =>
              val (block, syclEventRef) = rewriteSYCLQueueSubmit(inv)
              v = syclEventRef
              result = block
            case inv: CPPInvocation[Pre] if inv.ref.get.name == "sycl::buffer::constructor" =>
              val (block, syclBufferRef) = rewriteSYCLBufferConstruction(inv, Some(varO))
              v = syclBufferRef
              result = block
            case inv: CPPInvocation[Pre] if inv.ref.get.name == "sycl::local_accessor::constructor" =>
              val newValue = invocation(inv)
              v = new Variable(newValue.t)(v.o)
              result = Block(Seq(LocalDecl(v), assignLocal(v.get, newValue)))
            case local: CPPLocal[Pre] if typ.isInstanceOf[SYCLTEvent[Post]] =>
              v = new Variable(cppNameSuccessor(local.ref.get).t)(varO)
              result = Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(preValue))))
            case _ =>
              result = Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(preValue))))
          }
        }
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        result
    }
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
          val accessor = currentAccessorSubstitutions(ref)
          val deref = Deref[Post](currentThis.get, accessor.instanceField.ref)(SYCLAccessorFieldInsufficientReferencePermissionBlame(local))
          currentReplacementsForAccessors(deref) = accessor
          deref
        case _: SYCLTLocalAccessor[Pre] if currentKernelType.get.isInstanceOf[BasicKernel] => throw SYCLNoLocalAccessorsInBasicKernel(local)
        case _ => Local(cppNameSuccessor.ref(ref))
      }
      case RefSYCLAccessMode(decl) => rw.dispatch(decl)
    }
  }

  def preAssignExpr(preAssign: PreAssignExpression[Pre], target: CPPLocal[Pre]): Expr[Post] = {
    CPP.unwrappedType(target.t) match {
      case _: SYCLTEvent[Pre] => throw SYCLReassigningOfVariableUnsupported("event", "EventVariable", preAssign)
      case _: SYCLTBuffer[Pre] => throw SYCLReassigningOfVariableUnsupported("buffer", "Buffer", preAssign)
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
            val accessors: Seq[SYCLAccessor[Post]] = currentRunningKernels.getOrElse(localVar, throw Unreachable(inv.o.messageInContext("Could not find the event variable in the stored running kernels.")))
            syclKernelTermination(localVar, accessors)
          case _ => throw Unreachable(inv.o.messageInContext("The object on which the wait() method was called is not a locally declared SYCL event."))
        }

      case "sycl::queue::submit" => rewriteSYCLQueueSubmit(inv)._1
      case "sycl::buffer::constructor" => rewriteSYCLBufferConstruction(inv)._1
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
      case "sycl::range::constructor" => SYCLRange[Post](args.map(rw.dispatch))
      case "sycl::nd_range::constructor" => SYCLNDRange[Post](rw.dispatch(args.head), rw.dispatch(args(1)))
      case "sycl::item::get_id" => getSimpleWorkItemId(inv, GlobalScope())
      case "sycl::item::get_range" => getSimpleWorkItemRange(inv, GlobalScope())
      case "sycl::item::get_linear_id" => getSimpleWorkItemLinearId(inv, GlobalScope())
      case "sycl::nd_item::get_local_id" => getSimpleWorkItemId(inv, LocalScope())
      case "sycl::nd_item::get_local_range" => getSimpleWorkItemRange(inv, LocalScope())
      case "sycl::nd_item::get_local_linear_id" => getSimpleWorkItemLinearId(inv, LocalScope())
      case "sycl::nd_item::get_group_id" => getSimpleWorkItemId(inv, GroupScope())
      case "sycl::nd_item::get_group_range" => getSimpleWorkItemRange(inv, GroupScope())
      case "sycl::nd_item::get_group_linear_id" => getSimpleWorkItemLinearId(inv, GroupScope())
      case "sycl::nd_item::get_global_id" => getGlobalWorkItemId(inv)
      case "sycl::nd_item::get_global_range" => getGlobalWorkItemRange(inv)
      case "sycl::nd_item::get_global_linear_id" => getGlobalWorkItemLinearId(inv)
      case "sycl::accessor::get_range" if classInstance.isDefined =>
        val rangeIndexFields = currentReplacementsForAccessors(classInstance.get).rangeIndexFields
        LiteralSeq[Post](TInt(), rangeIndexFields.map(f => Deref[Post](currentThis.get, f.ref)(SYCLAccessorRangeIndexFieldInsufficientReferencePermissionBlame(inv))))
      case "sycl::accessor::get_range" => throw NotApplicable(inv)
      case "sycl::range::get" => (classInstance, args) match {
        case (Some(seq: LiteralSeq[Post]), Seq(arg)) => SeqSubscript(seq, rw.dispatch(arg))(SYCLRequestedRangeIndexOutOfBoundsBlame(seq, arg)) // Range coming from calling get_range() on an accessor
        case _ => throw NotApplicable(inv)
      }
      case "sycl::local_accessor::constructor" => {
        val range = rw.dispatch(args.head).asInstanceOf[SYCLRange[Post]]
        NewArray(rw.dispatch(inv.t.asInstanceOf[SYCLTLocalAccessor[Pre]].typ), range.dimensions, 0)(SYCLLocalAccessorArraySizeBlame(args.head))
      }

      case _ => {
        val procedureRef: Ref[Post, Procedure[Post]] = cppFunctionDeclSuccessor.ref((decls, initIdx))
        ProcedureInvocation[Post](
          procedureRef, args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) }
        )(inv.blame)
      }
    }
  }

  private def getSYCLHeaderMethodRef(name: String, args: Seq[Expr[Post]]): Ref[Post, Procedure[Post]] = {
    val declarationsOfName = savedGlobalDeclarations.getOrElse(name, throw SYCLHeaderItemNotFound("method", name))
    val index = declarationsOfName.indexWhere(
      tuple => Util.compatTypes[Post](args, CPP.getParamTypes[Pre](RefCPPGlobalDeclaration[Pre](tuple._1, 0)).map(rw.dispatch))
    )
    if (index == -1) throw SYCLHeaderItemNotFound("method", name)

    val (decl, alreadyDeclared) = declarationsOfName(index)
    if (!alreadyDeclared) {
      rewriteGlobalDecl(decl, mustDeclare = true)
      declarationsOfName.update(index, (decl, true))
    }

    cppFunctionDeclSuccessor.ref((decl, 0))
  }

  private def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): (Block[Post], Variable[Post]) = {
    // Do not allow given and yields on the invocation
    if (invocation.givenArgs.nonEmpty || invocation.yields.nonEmpty) throw SYCLGivenYieldsOnSYCLMethodsUnsupported(invocation)

    // Get the lambda describing the command group
    val commandGroup = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]]
    val commandGroupBody: Statement[Pre] = commandGroup.body
    val commandGroupBodyStatements: Seq[Statement[Pre]] = commandGroupBody.asInstanceOf[Scope[Pre]].body.asInstanceOf[CPPLifetimeScope[Pre]].body.asInstanceOf[Block[Pre]].statements

    // Do not allow contracts for the command group
    if (commandGroup.contract.nonEmpty) throw SYCLContractForCommandGroupUnsupported(commandGroup.contract)

    // Get the kernel declarations in the command group
    val collectedKernelDeclarations: Seq[CPPInvocation[Pre]] = {
      commandGroupBodyStatements.collect({
        case Eval(inv) if inv.isInstanceOf[CPPInvocation[Pre]] && inv.asInstanceOf[CPPInvocation[Pre]].ref.isDefined &&
          inv.asInstanceOf[CPPInvocation[Pre]].ref.get.name == "sycl::handler::parallel_for" =>
          inv.asInstanceOf[CPPInvocation[Pre]]
      })
    }

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
    val preClass: Class[Pre] = new Class(Nil, Nil, tt)(commandGroup.o)
    this.currentThis = Some(rw.dispatch(ThisObject[Pre](preClass.ref)(preClass.o)))

    // Generate conditions and accessor objects for each accessor declared in the command group
    val collectedAccessorDeclarations: Seq[CPPLocalDeclaration[Pre]] = commandGroupBodyStatements.collect({
      case Block(Seq(declStmnt: CPPDeclarationStatement[Pre])) if CPP.getBaseTypeFromSpecs(declStmnt.decl.decl.specs).isInstanceOf[SYCLTAccessor[Pre]] => declStmnt.decl
    })
    val (accessorRunMethodConditions, accessorParblockConditions, bufferAccessStatements, accessors) = rewriteSYCLAccessorDeclarations(collectedAccessorDeclarations)

    // Generate an array each local accessor declared in the command group
    val collectedLocalAccessorDeclarations: Seq[CPPLocalDeclaration[Pre]] = commandGroupBodyStatements.collect({
      case Block(Seq(declStmnt: CPPDeclarationStatement[Pre])) if CPP.getBaseTypeFromSpecs(declStmnt.decl.decl.specs).isInstanceOf[SYCLTLocalAccessor[Pre]] => declStmnt.decl
    })

    // Check that there is no other code in the command group other than 1 kernel declaration and the found (local) accessors
    if (commandGroupBodyStatements.size > 1 + accessors.size + collectedLocalAccessorDeclarations.size) {
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
    val runMethodPreCondition = {
      implicit val o: Origin = kernelDeclaration.contract.requires.o
      UnitAccountedPredicate(
        foldStar(accessorRunMethodConditions :+ getKernelQuantifiedCondition(kernelParBlock, removeKernelClassInstancePermissions(contractRequires)))(commandGroupBody.o)
      )
    }
    val runMethodPostCondition = {
      implicit val o: Origin = kernelDeclaration.contract.ensures.o
      UnitAccountedPredicate(
        foldStar(accessorRunMethodConditions :+ getKernelQuantifiedCondition(kernelParBlock, removeKernelClassInstancePermissions(contractEnsures)))(commandGroupBody.o)
      )
    }

    // Declare the newly generated kernel code inside a run-method
    val runMethodContract = ApplicableContract[Post](runMethodPreCondition, runMethodPostCondition, tt, Nil, Nil, Nil, None)(SYCLKernelRunMethodContractUnsatisfiableBlame(runMethodPreCondition))(commandGroup.o)
    val runMethod = new RunMethod[Post](
      body = Some(ParStatement[Post](kernelParBlock)(kernelDeclaration.body.o)),
      contract = runMethodContract,
    )(KernelLambdaRunMethodBlame(kernelDeclaration))(commandGroup.o)

    // Create the surrounding class
    val postClass = new Class[Post](
      declarations = accessors.flatMap(acc => acc.instanceField +: acc.rangeIndexFields) ++ Seq(runMethod),
      supports = Seq(),
      intrinsicLockInvariant = tt
    )(commandGroup.o.where(name = "SYCL_EVENT_CLASS"))
    rw.globalDeclarations.succeed(preClass, postClass)

    // Create a variable to refer to the class instance
    val classRef = new Variable[Post](TClass(postClass.ref))(commandGroup.o.where(name = "sycl_event_ref"))
    // Store the class ref and read-write accessors to be used when the kernel is done running
    currentRunningKernels.put(classRef.get(commandGroup.o), accessors.filter(acc => acc.accessMode.isInstanceOf[SYCLReadWriteAccess[Post]]))

    // Declare a constructor for the class as a separate global method
    val constructor = createClassConstructor(accessors, preClass, commandGroup.o)

    // Generate expressions that check the bounds of the given range
    val rangeSizeChecks = getRangeSizeChecks(kernelDimensions)

    // Reset the global variables as we are done processing the kernel
    currentKernelType = None
    currentDimensions.clear()
    currentAccessorSubstitutions.clear()
    currentThis = None

    // Create a new class instance and assign it to the class instance variable, then fork that variable
    (Block[Post](
      bufferAccessStatements ++
      Seq(
        rangeSizeChecks,
        LocalDecl[Post](classRef)(commandGroup.o),
        assignLocal(
          classRef.get(commandGroup.o),
          ProcedureInvocation[Post]( // Call constructor
            constructor.ref,
            accessors.flatMap(acc => acc.buffer.generatedVar.get(acc.buffer.generatedVar.o) +: acc.buffer.range.dimensions.map(dim => dim)),
            Nil, Nil, Nil, Nil,
          )(PanicBlame("The preconditions of a kernel class constructor cannot be unsatisfiable, as it does not have preconditions."))(commandGroup.o)
        )(commandGroup.o),
        Fork(classRef.get(classRef.o))(SYCLKernelForkBlame(kernelDeclaration))(invocation.o)
      )
    )(invocation.o), classRef)
  }

  private def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre], accessorParblockConditions: Seq[Expr[Post]]): (ParBlock[Post], Expr[Post], Expr[Post]) = {
    // Register the kernel dimensions
    val range: Seq[Expr[Post]] = rw.dispatch(kernelDimensions) match {
      case SYCLRange(dims) => dims
      case _ => throw Unreachable("The dimensions parameter of the kernel was not rewritten to a range.")
    }

    currentKernelType = Some(BasicKernel(range))
    currentDimensions(GlobalScope()) = range.indices.map(index => createRangeIterVar(GlobalScope(), index, range(index))(range(index).o))

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)

    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the kernels
    val parBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_BASIC_KERNEL")),
      iters = currentDimensions(GlobalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = foldStar(accessorParblockConditions :+ contractRequires),
      ensures = foldStar(accessorParblockConditions :+ contractEnsures),
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

    currentKernelType = Some(NDRangeKernel(globalRange, localRange))
    currentDimensions(LocalScope()) = localRange.indices.map(index => createRangeIterVar(LocalScope(), index, localRange(index))(localRange(index).o))
    currentDimensions(GroupScope()) = globalRange.indices.map(index =>
      createRangeIterVar(GroupScope(), index, FloorDiv(globalRange(index), localRange(index))(ImpossibleDivByZeroBlame())(localRange(index).o))(kernelDimensions.o))

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)

    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the work-items inside work-groups
    val workItemParBlock = ParStatement[Post](ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_ND_RANGE_KERNEL_WORKITEMS")),
      iters = currentDimensions(LocalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = foldStar(accessorParblockConditions :+ contractRequires),
      ensures = foldStar(accessorParblockConditions :+ contractEnsures),
      content = rw.dispatch(kernelDeclaration.body)
    )(SYCLKernelParBlockFailureBlame(kernelDeclaration)))

    val localAccessorDecls = localAccessorDeclarations.map(localAccDecl => rewriteLocalDecl(localAccDecl))
    val localAccessorVariables = localAccessorDeclarations.map(localAcc => cppNameSuccessor(RefCPPLocalDeclaration(localAcc, 0)))

    val quantifiedConstractRequires = getKernelQuantifiedCondition(workItemParBlock.impl.asInstanceOf[ParBlock[Post]], removeLocalAccessorConditions(contractRequires, localAccessorVariables))
    val quantifiedConstractEnsures = getKernelQuantifiedCondition(workItemParBlock.impl.asInstanceOf[ParBlock[Post]], removeLocalAccessorConditions(contractEnsures, localAccessorVariables))

    // Create the parblock representing the work-groups
    val workGroupParBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.where(name = "SYCL_ND_RANGE_KERNEL_WORKGROUPS")),
      iters = currentDimensions(GroupScope()),
      context_everywhere = tt,
      requires = foldStar(accessorParblockConditions :+ quantifiedConstractRequires),
      ensures = foldStar(accessorParblockConditions :+ quantifiedConstractEnsures),
      content = Scope(Nil, Block(localAccessorDecls :+ workItemParBlock))
    )(SYCLKernelParBlockFailureBlame(kernelDeclaration))

    (workGroupParBlock, quantifiedConstractRequires, quantifiedConstractEnsures)
  }

  private def rewriteSYCLAccessorDeclarations(decls: Seq[CPPLocalDeclaration[Pre]]): (Seq[Expr[Post]], Seq[Expr[Post]], Seq[Statement[Post]], Seq[SYCLAccessor[Post]]) = {
    val runMethodConditions: mutable.Buffer[Expr[Post]] = mutable.Buffer.empty
    val parblockConditions: mutable.Buffer[Expr[Post]] = mutable.Buffer.empty
    val bufferAccessStatements: mutable.Buffer[Statement[Post]] = mutable.Buffer.empty
    val accessors: mutable.Buffer[SYCLAccessor[Post]] = mutable.Buffer.empty
    decls.foreach(decl => {
      val accDecl = decl.decl
      val accName = CPP.nameFromDeclarator(accDecl.inits.head.decl)
      if (accDecl.inits.nonEmpty && accDecl.inits.head.init.isDefined && accDecl.inits.head.init.get.isInstanceOf[CPPInvocation[Pre]]) {
        val accO: Origin = SYCLGeneratedAccessorPermissionsOrigin(decl).where(name = accName)
        val dimO: Origin = SYCLGeneratedAccessorPermissionsOrigin(decl)
        accDecl.inits.head.init.get match {
          case inv@CPPInvocation(_, Seq(bufferRef: CPPLocal[Pre], _, accessModeRef: CPPLocal[Pre]), _, _) => {
            rw.dispatch(accessModeRef)  match {
              case accessMode: SYCLAccessMode[Post] =>
                val buffer = getFromAll(syclBufferSuccessor, cppNameSuccessor(bufferRef.ref.get)).getOrElse(throw SYCLBufferOutOfScopeError(decl))
                if (!CPP.getBaseTypeFromSpecs(accDecl.specs).asInstanceOf[SYCLTAccessor[Pre]].equals(CPP.unwrappedType(inv.t))) {
                  throw Unreachable("Accessor type does not correspond with buffer type!")
                }
                val instanceField = new InstanceField[Post](buffer.generatedVar.t, Set())(accO)
                val rangeIndexFields = Seq.range(0, buffer.range.dimensions.size).map(i => new InstanceField[Post](TInt(), Set())(dimO.where(name = s"${accName}_r$i")))
                accessors.append(SYCLAccessor[Post](buffer, accessMode, instanceField, rangeIndexFields)(accDecl.o))
                currentAccessorSubstitutions(RefCPPLocalDeclaration(decl, 0)) = accessors.last

                val (basicPermissions, fieldArrayElementsPermission) = getBasicAccessorPermissions(accessors.last, this.currentThis.get)
                parblockConditions.append(basicPermissions)
                runMethodConditions.append(basicPermissions)
                runMethodConditions.append(fieldArrayElementsPermission)

                bufferAccessStatements.append(getBufferAccess(buffer, accessMode, decl))

              case _ => throw Unreachable(accessModeRef.o.messageInContext("Accesmode was rewritten to something invalid."))
            }
          }
          case _ => throw Unreachable(accDecl.inits.head.init.get.o.messageInContext("Accessor declaration is malformed."))
        }

      } else {
        throw Unreachable(decl.o.messageInContext("Accessor declaration is malformed."))
      }
    })
    (runMethodConditions.toSeq, parblockConditions.toSeq, bufferAccessStatements.toSeq, accessors.toSeq)
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

  private def createClassConstructor(accessors: Seq[SYCLAccessor[Post]], preClass: Class[Pre], commandGroupO: Origin): Procedure[Post] = {
    val t = rw.dispatch(TClass[Pre](preClass.ref))
    rw.globalDeclarations.declare(withResult((result: Result[Post]) => {
      val constructorPostConditions: mutable.Buffer[Expr[Post]] = mutable.Buffer.empty
      val constructorArgs: mutable.Buffer[Variable[Post]] = mutable.Buffer.empty

      accessors.foreach(acc => {
        val newConstructorAccessorArg = new Variable[Post](TArray(TInt()))(acc.instanceField.o)
        val newConstructorDimensionArgs = acc.rangeIndexFields.map(f => new Variable[Post](TInt())(f.o))

        val (basicPermissions, fieldArrayElementsPermission)  = getBasicAccessorPermissions(acc, result)
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
              Local[Post](newConstructorDimensionArgs(i).ref)(newConstructorDimensionArgs(i).o)
            )(newConstructorDimensionArgs(i).o)
          )
        )(acc.o))

        constructorArgs.append(newConstructorAccessorArg)
        constructorArgs.appendAll(newConstructorDimensionArgs)
      })

      {
        implicit val o: Origin = commandGroupO
        new Procedure[Post](
          returnType = t,
          args = constructorArgs.toSeq,
          outArgs = Seq(), typeArgs = Seq(), body = None,
          contract = ApplicableContract[Post](
            UnitAccountedPredicate[Post](tt),
            SplitAccountedPredicate(
              left = UnitAccountedPredicate((result !== Null()) && (TypeOf(result) === TypeValue(t))),
              right = UnitAccountedPredicate[Post](foldStar[Post](constructorPostConditions.toSeq :+ IdleToken[Post](result))),
            ),
            tt, Seq(), Seq(), Seq(), None
          )(PanicBlame("Constructors of kernel classes do not have pre-conditions, so it is impossible for them to be unsatisfiable."))
        )(SYCLKernelConstructorCallableFailureBlame())(o.where(name = "event_constructor"))
      }
    })(commandGroupO))
  }

  private def getRangeSizeChecks(range: Expr[Pre]): Assert[Post] = currentKernelType.get match {
    case BasicKernel(globalRangeSizes) =>
      Assert(
        foldStar(globalRangeSizes.map(expr => {
          implicit val o: Origin = RangeDimensionCheckOrigin(expr)
          GreaterEq(expr, const(0))
        }))(RangeDimensionCheckOrigin(range))
      )(SYCLKernelRangeInvalidBlame())(RangeDimensionCheckOrigin(range))
    case NDRangeKernel(globalRangeSizes, localRangeSizes) =>
      Assert(
        foldStar(globalRangeSizes.indices.map(i => {
          implicit val o: Origin = NDRangeDimensionCheckOrigin(range, Some(i))
          And(
            And(
              Greater(localRangeSizes(i), const(0)),
              GreaterEq(globalRangeSizes(i), const(0))
            ),
            Eq(
              Mod(globalRangeSizes(i), localRangeSizes(i))(ImpossibleDivByZeroBlame()),
              const(0)
            )
          )
        }
        ))(NDRangeDimensionCheckOrigin(range))
      )(SYCLKernelRangeInvalidBlame())(RangeDimensionCheckOrigin(range))
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
    val variable = new Variable[Post](TInt())(o.where(name = s"${scope.idName}_$dimension"))
    new IterVariable[Post](variable, IntegerValue(0), maxRange)
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
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq((procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(level).map(iterVar => iterVar.variable.get))))

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getSimpleWorkItemRange(inv: CPPInvocation[Pre], level: KernelScopeLevel)(implicit o: Origin): Expr[Post] = {
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq((procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(level).map(iterVar => iterVar.to))))

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getSimpleWorkItemLinearId(inv: CPPInvocation[Pre], level: KernelScopeLevel)(implicit o: Origin): Expr[Post] = {
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq(
        (procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(level).map (iterVar => iterVar.variable.get))),
        (procedureRef.decl.contract.givenArgs(1).ref, LiteralSeq(TInt(), currentDimensions(level).map(iterVar => iterVar.to)))
      )

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getGlobalWorkItemId(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq(
        (procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(GroupScope()).map(iterVar => iterVar.variable.get))),
        (procedureRef.decl.contract.givenArgs(1).ref, LiteralSeq(TInt(), currentDimensions(LocalScope()).map(iterVar => iterVar.variable.get))),
        (procedureRef.decl.contract.givenArgs(2).ref, LiteralSeq(TInt(), currentDimensions(GroupScope()).map(iterVar => iterVar.to)))
      )

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getGlobalWorkItemRange(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq(
        (procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(GroupScope()).map(iterVar => iterVar.to))),
        (procedureRef.decl.contract.givenArgs(1).ref, LiteralSeq(TInt(), currentDimensions(LocalScope()).map(iterVar => iterVar.to)))
      )

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getGlobalWorkItemLinearId(inv: CPPInvocation[Pre])(implicit o: Origin): Expr[Post] = {
    val givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])] = procedureRef =>
      Seq(
        (procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(GroupScope()).map(iterVar => iterVar.variable.get))),
        (procedureRef.decl.contract.givenArgs(1).ref, LiteralSeq(TInt(), currentDimensions(LocalScope()).map(iterVar => iterVar.variable.get))),
        (procedureRef.decl.contract.givenArgs(2).ref, LiteralSeq(TInt(), currentDimensions(GroupScope()).map(iterVar => iterVar.to))),
        (procedureRef.decl.contract.givenArgs(3).ref, LiteralSeq(TInt(), currentDimensions(LocalScope()).map(iterVar => iterVar.to)))
      )

    getSYCLWorkItemIdOrRange(inv, givenValueLambda)
  }

  private def getSYCLWorkItemIdOrRange(inv: CPPInvocation[Pre], givenValueLambda: Ref[Post, Procedure[Post]] => Seq[(Ref[Post, Variable[Post]], Expr[Post])])(implicit o: Origin) : Expr[Post] = {
    val RefCPPGlobalDeclaration(decls, initIdx) = inv.ref.get
    val procedureRef: Ref[Post, Procedure[Post]] = cppFunctionDeclSuccessor.ref((decls, initIdx))
    val givenValue: Seq[(Ref[Post, Variable[Post]], Expr[Post])] = currentKernelType match {
      case None => inv.givenArgs.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) }
      case Some(_) => givenValueLambda(procedureRef)
    }

    ProcedureInvocation[Post](
      procedureRef, inv.args.map(rw.dispatch), Nil, Nil,
      givenValue,
      Seq()
    )(SYCLItemMethodInvocationBlame(inv))
  }

  private def rewriteSYCLBufferConstruction(inv: CPPInvocation[Pre], maybeVarNameO: Option[Origin] = None): (Statement[Post], Variable[Post]) = {
    implicit val o: Origin = inv.o
    val varNameO = maybeVarNameO.getOrElse(o)
    val array = rw.dispatch(inv.args.head)

    val range = rw.dispatch(inv.args(1)) match {
      case r: SYCLRange[Post] => r
      case _ => throw Unreachable("The dimensions parameter of the kernel was not rewritten to a range.")
    }

    val v = new Variable[Post](FuncTools.repeat(TArray[Post](_), 1, array.t.asPointer.get.element))(varNameO)
    val args = Seq(array, range.size)

    // Call the method that copies the hostData contents to the buffer
    val copyInvRef = getSYCLHeaderMethodRef("sycl::buffer::copy_hostdata_to_buffer", args)
    val copyInv = ProcedureInvocation(copyInvRef, args, Nil, Nil, Nil, Nil)(SYCLBufferConstructionInvocationBlame(inv))

    // Fold predicate to gain exclusive access to the hostData
    val exclusiveAccessPred = savedPredicates.find(pred =>
        Util.compatTypes[Post](args, CPP.getParamTypes[Pre](RefPredicate[Pre](pred)).map(rw.dispatch))
    ).getOrElse(throw SYCLHeaderItemNotFound("predicate", "sycl::buffer::exclusive_hostData_access"))
    val gainExclusiveAccess = Fold(PredicateApply[Post](rw.succ(exclusiveAccessPred), args, WritePerm()))(SYCLBufferConstructionFoldFailedBlame(inv))

    val bufferLockVar = new Variable[Post](TInt())(varNameO.where(name = varNameO.getPreferredNameOrElse().ucamel + "_lock"))

    val result: Statement[Post] = Block(Seq(LocalDecl(v), assignLocal(v.get, copyInv), gainExclusiveAccess, LocalDecl(bufferLockVar), assignLocal(bufferLockVar.get, const(0))))
    syclBufferSuccessor.top.put(v, SYCLBuffer(array, v, range, bufferLockVar))
    (result, v)
  }

  private def destroySYCLBuffer(buffer: SYCLBuffer[Post], scope: CPPLifetimeScope[_]): Statement[Post] = {
    implicit val o: Origin = buffer.o

    // Wait for SYCL kernels that access the buffer to finish executing
    val kernelsToTerminate = currentRunningKernels.filter(tuple => tuple._2.exists(acc => acc.buffer.equals(buffer)))
    val kernelTerminations = kernelsToTerminate.map(tuple => syclKernelTermination(tuple._1, tuple._2)).toSeq

    // Call the method that copies the buffer contents to the hostData
    val copyInvRef = getSYCLHeaderMethodRef("sycl::buffer::copy_buffer_to_hostdata", Seq(buffer.hostData, buffer.generatedVar.get))
    val copyInv = ProcedureInvocation(copyInvRef, Seq(buffer.hostData, buffer.generatedVar.get), Nil, Nil, Nil, Nil)(SYCLBufferDestructionInvocationBlame(buffer.generatedVar, scope))

    val args = Seq(buffer.hostData, buffer.range.size)

    // Unfold predicate to release exclusive access to the hostData
    val exclusiveAccessPred = savedPredicates.find(pred =>
      Util.compatTypes[Post](args, CPP.getParamTypes[Pre](RefPredicate[Pre](pred)).map(rw.dispatch))
    ).getOrElse(throw SYCLHeaderItemNotFound("predicate", "sycl::buffer::exclusive_hostData_access"))
    val removeExclusiveAccess = Unfold(PredicateApply[Post](rw.succ(exclusiveAccessPred), args, WritePerm()))(SYCLBufferDestructionUnfoldFailedBlame(buffer.generatedVar, scope))

    Block(kernelTerminations ++ Seq(removeExclusiveAccess, Eval(copyInv)))
  }

  private def getBufferAccess(buffer: SYCLBuffer[Post], mode: SYCLAccessMode[Post], source: CPPLocalDeclaration[Pre]): Statement[Post] = {
    implicit val o: Origin = source.o
    val (maxValue, newValue: Expr[Post]) = mode match {
      case SYCLReadOnlyAccess() => (Greater(buffer.bufferLockVar.get, const(-1)), Plus(buffer.bufferLockVar.get, const(1)))
      case SYCLReadWriteAccess() => (Eq(buffer.bufferLockVar.get, const(0)), const(-1))
    }
    Block[Post](Seq(
      Assert(maxValue)(SYCLBufferLockBlame(source)),
      assignLocal(buffer.bufferLockVar.get, newValue)
    ))
  }

  private def releaseBufferAccess(acc: SYCLAccessor[Post]): Statement[Post] = {
    implicit val o: Origin = acc.o
    val newValue: Expr[Post] = acc.accessMode match {
      case SYCLReadOnlyAccess() => Minus(acc.buffer.bufferLockVar.get, const(1))
      case SYCLReadWriteAccess() => const(0)
    }
    assignLocal(acc.buffer.bufferLockVar.get, newValue)
  }

  def rewriteAccessorSubscript(sub: AmbiguousSubscript[Pre]): Expr[Post] = sub match {
    case AmbiguousSubscript(base: CPPLocal[Pre], index) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 1) => ArraySubscript[Post](
          Deref[Post](
            currentThis.get,
            currentAccessorSubstitutions(base.ref.get).instanceField.ref
          )(new SYCLAccessorDerefBlame(currentAccessorSubstitutions(base.ref.get).instanceField))(sub.o),
          rw.dispatch(index)
        )(SYCLAccessorArraySubscriptErrorBlame(sub))(sub.o)
        case t: SYCLTAccessor[Pre] => throw SYCLWrongNumberOfSubscriptsForAccessor(sub, 1, t.dimCount)
        case _ => ???
      }
    case AmbiguousSubscript(AmbiguousSubscript(base: CPPLocal[Pre], indexX), indexY) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      implicit val o: Origin = sub.o
      val accessor = currentAccessorSubstitutions(base.ref.get)
      val linearizeArgs = Seq(rw.dispatch(indexX), rw.dispatch(indexY),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(0).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(0))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(1).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(1)))
      )
      val linearizedIndexProc = getSYCLHeaderMethodRef("sycl::accessor::linearize_2_indices", linearizeArgs)
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 2) => ArraySubscript[Post](
          Deref[Post](currentThis.get, accessor.instanceField.ref)(new SYCLAccessorDerefBlame(accessor.instanceField)),
          ProcedureInvocation[Post](linearizedIndexProc, linearizeArgs, Nil, Nil, Nil, Nil)(SYCLAccessorArraySubscriptLinearizeInvocationBlame(sub, base, Seq(indexX, indexY)))
        )(SYCLAccessorArraySubscriptErrorBlame(sub))
        case t: SYCLTAccessor[Pre] => throw SYCLWrongNumberOfSubscriptsForAccessor(sub, 2, t.dimCount)
        case _ => ???
      }
    case AmbiguousSubscript(AmbiguousSubscript(AmbiguousSubscript(base: CPPLocal[Pre], indexX), indexY), indexZ) if CPP.unwrappedType(base.t).isInstanceOf[SYCLTAccessor[Pre]] =>
      implicit val o: Origin = sub.o
      val accessor = currentAccessorSubstitutions(base.ref.get)
      val linearizeArgs = Seq(rw.dispatch(indexX), rw.dispatch(indexY), rw.dispatch(indexZ),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(0).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(0))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(1).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(1))),
        Deref[Post](currentThis.get, accessor.rangeIndexFields(2).ref)(new SYCLAccessorDimensionDerefBlame(accessor.rangeIndexFields(2)))
      )
      val linearizedIndexProc = getSYCLHeaderMethodRef("sycl::accessor::linearize_3_indices", linearizeArgs)
      CPP.unwrappedType(base.t) match {
        case SYCLTAccessor(_, 3) => ArraySubscript[Post](
          Deref[Post](currentThis.get, accessor.instanceField.ref)(new SYCLAccessorDerefBlame(accessor.instanceField)),
          ProcedureInvocation[Post](linearizedIndexProc, linearizeArgs, Nil, Nil, Nil, Nil)(SYCLAccessorArraySubscriptLinearizeInvocationBlame(sub, base, Seq(indexX, indexY, indexZ)))
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
    currentRunningKernels.remove(variable)
    Block(
      Join(variable)(SYCLKernelJoinBlame()) +:
      accessors.flatMap(acc => acc.accessMode match {
        case SYCLReadWriteAccess() => Seq(
          // Only copy data back if there was read-write access
          assignLocal(
            acc.buffer.generatedVar.get,
            Deref[Post](variable, acc.instanceField.ref)(new SYCLAccessorDerefBlame(acc.instanceField))
          ),
          releaseBufferAccess(acc)
        )
        case SYCLReadOnlyAccess() => Seq(releaseBufferAccess(acc))
      })
    )
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
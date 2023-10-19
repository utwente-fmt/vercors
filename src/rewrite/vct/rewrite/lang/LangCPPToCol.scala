package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin.messagesInContext
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.rewrite.ParBlockEncoder.ParBlockNotInjective
import vct.col.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.rewrite.{Generation, ParBlockEncoder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, SuccessionMap}
import vct.result.VerificationError.UserError

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
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }

  private case class ReassigningEventVars(ass: PreAssignExpression[_]) extends UserError {
    override def code: String = "unsupportedReassigningOfEventVars"
    override def text: String = ass.o.messageInContext(s"Reassigning variables holding a SYCL event is not supported.")
  }

  private case class SYCLEventWaitWrongType(inv: Expr[_]) extends UserError {
    override def code: String = "unexpectedSYCLEventWaitWrongType"
    override def text: String = inv.o.messageInContext("The declarator for the invocation to sycl::event::wait() is not a SYCL event.")
  }

  private case class ContractForCommandGroup(contract: ApplicableContract[_]) extends UserError {
    override def code: String = "unsupportedContractForCommandGroup"
    override def text: String = contract.o.messageInContext(s"Contracts are not supported for SYCL command groups.")
  }

  private case class GivenYieldsOnSYCLMethods(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "unsupportedGivenYieldsOnSYCLMethods"
    override def text: String = inv.o.messageInContext(s"Given and yields annotations are not supported for calls to built-in SYCL methods.")
  }

  private case class MultipleKernels(decl1: Statement[_], decl2: CPPInvocation[_]) extends UserError {
    override def code: String = "multipleKernels"
    override def text: String = Origin.messagesInContext(Seq(
      decl2.o -> "This kernel declaration is not allowed, as only one kernel declaration is allowed per command group, ...",
      decl1.o -> "... and there is already a kernel declared here.",
    ))
  }

  private case class MissingKernel(decl: Statement[_]) extends UserError {
    override def code: String = "missingKernel"
    override def text: String = decl.o.messageInContext(s"There is no kernel declaration in this command group.")
  }

  private case class NonSYCLCodeInCommandGroup(decl: Statement[_]) extends UserError {
    override def code: String = "nonSYCLCodeInCommandGroup"
    override def text: String = decl.o.messageInContext(s"Non-SYCL code inside command group declarations is not supported.")
  }

  private case class IncorrectParallelForLambdaArgument(o: Origin) extends UserError {
    override def code: String = "incorrectParallelForLambdaArgument"
    override def text: String = o.messageInContext("The second parameter for the parallel_for method, which is a lambda method, " +
      "only takes one (nd_)item argument, which should match the first (nd_)range parameter of the parallel_for method.")
  }

  private case class WrongKernelDimensionType(expr: Expr[_]) extends UserError {
    override def code: String = "wrongKernelDimensionType"
    override def text: String = expr.o.messageInContext("Wrong type for the dimensions parameter of the kernel. " +
      "The dimensions parameter in a kernel declaration is supposed to be of type sycl::range<int> or sycl::nd_range<int>.")
  }

  private case class NotKernelDimensions(expr: Expr[_]) extends UserError {
    override def code: String = "unexpectedNotKernelDimensions"
    override def text: String = expr.o.messageInContext("The dimensions parameter of the kernel was not rewritten to a (nd_)range.")
  }

  private case class SYCLItemMethodInvocationBlame(inv: CPPInvocation[_]) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(_, _, node) => inv.blame.blame(SYCLItemMethodPreconditionFailed(node))
      case ContextEverywhereFailedInPre(_, _) => PanicBlame("Item methods do not contain context_everywhere clauses, so cannot fail on a context_everywhere clause.").blame(error)
      case SYCLItemMethodPreconditionFailed(_) => PanicBlame("Failing on a SYCLItemMethodPreconditionFailed error should not be possible here.").blame(error)
    }
  }

  private case class KernelForkNull(node: Fork[_]) extends UserError {
    override def code: String = "kernelForkNull"
    override def text: String = node.o.messageInContext("This event variable might not be linked to a kernel submission to a queue.")
  }

 private class RangeDimensionCheckOrigin(iterVarOrRange: Expr[_]) extends Origin(
   Seq(
     PreferredName("RangeDimensionCheck"),
     ShortPosition("generated"),
     iterVarOrRange.o.getContext.getOrElse(Context("[unknown context]")),
     iterVarOrRange.o.getInlineContext.getOrElse(InlineContext("[unknown inline context]")),
   )
  )

  private class NDRangeDimensionCheckOrigin(range: Expr[_], dimension: Option[Int] = None) extends Origin(
    Seq(
      PreferredName("NDRangeDimensionCheck"),
      ShortPosition("generated"),
      Context((if (dimension.isDefined) s"At the dimensions at index ${dimension.get} in the global and local range of the nd_range constructor. \n" else "") + range.o.getInlineContextOrElse()),
      InlineContext((if (dimension.isDefined) s" The dimensions at index ${dimension.get} in the global and local range of the nd_range constructor. \n" else "") + range.o.getInlineContextOrElse()),
    )
  )

  private case class KernelPreconditionNotEstablished(error: RunnablePreconditionNotEstablished) extends UserError {
    override def code: String = "kernelForkPre"
    override def text: String =  (error.failure, error.failure.node.o) match {
      case (ContractFalse(_), o: RangeDimensionCheckOrigin) => o.messageInContext("All range dimensions should be greater or equal to zero.")
      case (ContractFalse(_), o: NDRangeDimensionCheckOrigin) => o.messageInContext(
        "Every global range dimension should be divisible by the local range dimension at the same index," +
        " and the local range dimension should be greater than 0 to avoid division by zero. " +
        "All global range dimensions should be greater or equal to zero.")
      case _ => messagesInContext(Seq((error.node.o, "The precondition of the kernel may not hold, since ..."), (error.failure.node.o, "... " + error.failure.descCompletion)))
    }
  }

  private case class KernelForkBlame(kernel: CPPLambdaDefinition[_]) extends Blame[ForkFailure] {
    override def blame(error: ForkFailure): Unit = error match {
      case ForkNull(node) => throw KernelForkNull(node)
      case RunnableNotIdle(_) => throw BlameUnreachable("This kernel submission has been forked already. This should not happen.", error) // EW TODO also turn into user error?
      case e@RunnablePreconditionNotEstablished(_, _) => throw KernelPreconditionNotEstablished(e)
    }
  }

  private case class KernelJoinBlame() extends Blame[JoinFailure] {
    private case class KernelJoinError(error: JoinFailure) extends UserError {
      override def code: String = "kernelJoinNull"
      override def text: String = "This event variable might be null or no longer be linked to a kernel submission to a queue."
    }
    override def blame(error: JoinFailure): Unit = throw KernelJoinError(error)
  }

  private case class KernelParFailure(kernel: CPPLambdaDefinition[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
      case ParPredicateNotInjective(_, predicate) =>
        kernel.blame.blame(KernelLambdaFailure(KernelPredicateNotInjective(Right(kernel), predicate)))
      case ParPreconditionFailed(_, _) =>
        PanicBlame("Kernel parallel block precondition cannot fail, since an identical predicate is required before.").blame(error)
      case ParBlockPostconditionFailed(failure, _) =>
        kernel.blame.blame(KernelLambdaFailure(KernelPostconditionFailed(failure, Right(kernel))))
      case ParBlockMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
    }
  }

  private case class KernelRangeDivByZero(kernelDims: Expr[_]) extends Blame[DivByZero] {
    override def blame(error: DivByZero): Unit =
      PanicBlame(kernelDims.o.messageInContext("One or more dimensions in local range are zero, " +
        "which is not allowed as the global range is divided by the local range to retrieve the group range.")).blame(error)
  }

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

  sealed abstract class KernelScopeLevel(val idName: String)
  case class GlobalScope() extends KernelScopeLevel("GLOBAL_ID")
  case class LocalScope() extends KernelScopeLevel("LOCAL_ID")
  case class GroupScope() extends KernelScopeLevel("GROUP_ID")

  sealed abstract class KernelType()
  case class BasicKernel(globalRangeSizes: Seq[Expr[Post]]) extends KernelType()
  case class NDRangeKernel(globalRangeSizes: Seq[Expr[Post]], localRangeSizes: Seq[Expr[Post]]) extends KernelType()

  var currentDimensions: mutable.Map[KernelScopeLevel, Seq[IterVariable[Post]]] = mutable.Map.empty
  var currentKernelType: Option[KernelType] = None

  private def getRangeSizeChecks(range: Expr[Pre]): Expr[Post] = currentKernelType match {
    case Some(BasicKernel(globalRangeSizes)) =>
      foldStar(globalRangeSizes.map(expr => {
        implicit val o: Origin = new RangeDimensionCheckOrigin(expr)
        GreaterEq(expr, const(0))
      }))(new RangeDimensionCheckOrigin(range))
    case Some(NDRangeKernel(globalRangeSizes, localRangeSizes)) => foldStar(globalRangeSizes.indices.map(i =>
      {
        implicit val o: Origin = new NDRangeDimensionCheckOrigin(range, Some(i))
        And(
          And(
            Greater(localRangeSizes(i), const(0)),
            GreaterEq(globalRangeSizes(i), const(0))
          ),
          Eq(
            Mod(globalRangeSizes(i), localRangeSizes(i))(KernelRangeDivByZero(localRangeSizes(i))),
            const(0)
          )
        )
      }
    ))(new NDRangeDimensionCheckOrigin(range))
  }

  def rewriteUnit(cppUnit: CPPTranslationUnit[Pre]): Unit = {
    cppUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cppParam: CPPParam[Pre]): Unit = {
    cppParam.drop()
    val varO = cppParam.o.replacePrefName(CPP.getDeclaratorInfo(cppParam.declarator).name)

    val v = new Variable[Post](cppParam.specifiers.collectFirst
      { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get)(varO)
    cppNameSuccessor(RefCPPParam(cppParam)) = v
    rw.variables.declare(v)
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

    val namedO = func.o.replacePrefName(info.name)
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
      // If name starts with sycl:: and not with sycl::item or sycl::nd_item, it can be removed
      // because those built-in SYCL methods are just used for resolution and type checking
      if (!info.name.startsWith("sycl::") || info.name.startsWith("sycl::item") || info.name.startsWith("sycl::nd_item")) {
        val namedO = init.o.replacePrefName(info.name)
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
          case None =>
            cppGlobalNameSuccessor(RefCPPGlobalDeclaration(decl, idx)) =
              rw.globalDeclarations.declare(new HeapVariable(t)(namedO))
        }
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
    val varO: Origin = init.o.replacePrefName(info.name)
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
              val (block, syclEventRef) = rewriteSYCLQueueSubmit(init.init.get.asInstanceOf[CPPInvocation[Pre]])
              v = syclEventRef
              result = block
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

  def local(local: Either[CPPLocal[Pre], CPPClassInstanceLocal[Pre]]): Expr[Post] = {
    val (ref, blame, localExpr: Expr[Pre]) = local match {
      case Left(loc) => (loc.ref.get, loc.blame, loc)
      case Right(loc) => (loc.classLocalRef.get, loc.blame, loc)
    }
    implicit val o: Origin = localExpr.o
    ref match {
      case spec: SpecNameTarget[Pre] => rw.specLocal(spec, localExpr, blame)
      case _: SpecInvocationTarget[Pre] => throw NotAValue(localExpr)
      case ref: RefCPPParam[Pre] =>
        if (cppCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cppNameSuccessor.ref(RefCPPParam(cppCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cppNameSuccessor.ref(ref))
      case RefCPPFunctionDefinition(_) => throw NotAValue(localExpr)
      case ref @ RefCPPGlobalDeclaration(decl, initIdx) =>
        CPP.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None => DerefHeapVariable[Post](cppGlobalNameSuccessor.ref(ref))(blame)
          case Some(_) => throw NotAValue(localExpr)
        }
      case ref: RefCPPLocalDeclaration[Pre] => Local(cppNameSuccessor.ref(ref))
    }
  }

  def preAssignExpr(preAssign: PreAssignExpression[Pre], target: CPPLocal[Pre]): Expr[Post] = {
    target.t match {
      case primitive: CPPPrimitiveType[Pre] if CPP.getBaseTypeFromSpecs(primitive.specifiers).isInstanceOf[SYCLTEvent[Pre]] =>
        throw ReassigningEventVars(preAssign)
      case _ => rw.rewriteDefault(preAssign)
    }
  }

  def invocationStatement(eval: Eval[Pre]): Statement[Post] = {
    val inv = eval.expr.asInstanceOf[CPPInvocation[Pre]]
    inv.ref.get.name match {
      case "sycl::event::wait" if inv.applicable.isInstanceOf[CPPClassInstanceLocal[Pre]] =>
        implicit val o: Origin = inv.o
        val classRef = inv.applicable.asInstanceOf[CPPClassInstanceLocal[Pre]].classInstanceRef.get
        cppNameSuccessor.get(classRef) match {
          case Some(v) =>
            Join(v.get)(KernelJoinBlame())
          case _ => throw SYCLEventWaitWrongType(inv.applicable)
        }
      case "sycl::queue::submit" => rewriteSYCLQueueSubmit(inv)._1
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
    val CPPInvocation(_, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o
    
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

  private def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): (Block[Post], Variable[Post]) = {
    // Do not allow given and yields on the invocation
    if (invocation.givenArgs.nonEmpty || invocation.yields.nonEmpty) throw GivenYieldsOnSYCLMethods(invocation)

    // Get the lambda describing the command group
    val commandGroup = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]]
    val commandGroupBody: Statement[Pre] = commandGroup.body

    // Do not allow contracts for the command group
    if (commandGroup.contract.nonEmpty) throw ContractForCommandGroup(commandGroup.contract)

    // Get the kernel declarations in the command group
    val collectedKernelDeclarations: Seq[CPPInvocation[Pre]] = {
      commandGroupBody.asInstanceOf[Scope[Pre]].body.asInstanceOf[Block[Pre]].statements.collect({
        case Eval(inv) if inv.isInstanceOf[CPPInvocation[Pre]] && inv.asInstanceOf[CPPInvocation[Pre]].ref.isDefined &&
          inv.asInstanceOf[CPPInvocation[Pre]].ref.get.name == "sycl::handler::parallel_for" =>
          inv.asInstanceOf[CPPInvocation[Pre]]
      })
    }

    // Make sure there is only one kernel declaration in the command group
    if (collectedKernelDeclarations.isEmpty){
      throw MissingKernel(commandGroupBody)
    } else if (collectedKernelDeclarations.size > 1) {
      throw MultipleKernels(commandGroupBody, collectedKernelDeclarations(1))
    } else if (commandGroupBody.asInstanceOf[Scope[Pre]].body.asInstanceOf[Block[Pre]].statements.size > 1) {
      // Do not allow any code in the body except from parallel_for methods
      throw NonSYCLCodeInCommandGroup(commandGroupBody)
    } else if (collectedKernelDeclarations.head.givenArgs.nonEmpty || collectedKernelDeclarations.head.yields.nonEmpty) {
      // Do not allow given and yields on the kernel
      throw GivenYieldsOnSYCLMethods(collectedKernelDeclarations.head)
    }

    val kernelDimensions = collectedKernelDeclarations.head.args.head
    val kernelDeclaration = collectedKernelDeclarations.head.args(1).asInstanceOf[CPPLambdaDefinition[Pre]]

    // Get the kernel range type (which also checks if it is valid)
    val rangeType = getKernelRangeType(kernelDimensions.t, kernelDeclaration.declarator)

    // Create a block of code for the kernel body based on what type of kernel it is
    val (kernelParBlock, contractRequires, contractEnsures) = rangeType match {
      case SYCLTRange(_) => createBasicKernelBody(kernelDimensions, kernelDeclaration)
      case SYCLTNDRange(_) => createNDRangeKernelBody(kernelDimensions, kernelDeclaration)
      case _ => throw WrongKernelDimensionType(kernelDimensions)
    }

    // Create the pre- and postconditions for the method that will hold the generated kernel code
    val preCondition = getKernelQuantifiedCondition(kernelParBlock, contractRequires)(kernelDeclaration.contract.requires.o)
    val postCondition = getKernelQuantifiedCondition(kernelParBlock, contractEnsures)(kernelDeclaration.contract.ensures.o)
    val preConditionWithRangeChecks = Star(getRangeSizeChecks(kernelDimensions), preCondition)(kernelDimensions.o)

    // Create a contract for the method that will hold the generated kernel code
    val methodContract = {
      implicit val o: Origin = commandGroupBody.o
      ApplicableContract[Post](
        UnitAccountedPredicate(preConditionWithRangeChecks)(kernelDeclaration.contract.requires.o),
        UnitAccountedPredicate(postCondition)(kernelDeclaration.contract.ensures.o),
        AstBuildHelpers.foldStar(Seq()),
        Seq(), Seq(), Seq(), None
      )(kernelDeclaration.contract.blame)
    }

    implicit val o: Origin = kernelDeclaration.o

    // Declare the newly generated kernel code inside a run method
    val kernelBody = ParStatement[Post](kernelParBlock)(kernelDeclaration.body.o)
    val runMethod = new RunMethod[Post](
      body = Some(kernelBody),
      contract = methodContract,
    )(kernelDeclaration.blame)(o)

    // Reset the kernel type as we are done processing the kernel
    currentKernelType = None

    // Create a new class to hold the kernel method
    val classWrapper = rw.globalDeclarations.declare({
      rw.labelDecls.scope {
        new Class[Post](
          Seq(runMethod),
          Seq(),
          BooleanValue(value = true)
        )(o.replacePrefName("SYCL_EVENT_CLASS"))
      }
    })

    // Create a variable to refer to the class instance
    val variable = new Variable[Post](TClass(classWrapper.ref))(o.replacePrefName("sycl_event_ref"))

    // Create a new class instance and assign it to the class instance variable, then fork that variable
    (Block[Post](Seq(
      LocalDecl[Post](variable),
      assignLocal(variable.get, NewObject[Post](classWrapper.ref)),
      Fork(variable.get)(KernelForkBlame(kernelDeclaration))
    )), variable)
  }

  private def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): (ParBlock[Post], Expr[Post], Expr[Post]) = {
    // Register the kernel dimensions
    val range: Seq[Expr[Post]] = rw.dispatch(kernelDimensions) match {
      case SYCLRange(dims) => dims
      case _ => throw NotKernelDimensions(kernelDimensions)
    }

    currentKernelType = Some(BasicKernel(range))
    currentDimensions(GlobalScope()) = range.indices.map(index => createRangeIterVar(GlobalScope(), index, range(index))(range(index).o))

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)

    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the kernels
    val parBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.replacePrefName("SYCL_BASIC_KERNEL")),
      iters = currentDimensions(GlobalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = contractRequires,
      ensures = contractEnsures,
      content = rw.dispatch(kernelDeclaration.body),
    )(KernelParFailure(kernelDeclaration))

    (parBlock, contractRequires, contractEnsures)
  }

  private def createNDRangeKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): (ParBlock[Post], Expr[Post], Expr[Post]) = {
    // Register the kernel dimensions
    val (globalRange, localRange): (Seq[Expr[Post]], Seq[Expr[Post]]) = rw.dispatch(kernelDimensions) match {
      case SYCLNDRange(globalSize: SYCLRange[Post], localRange: SYCLRange[Post]) => (globalSize.dimensions, localRange.dimensions)
      case _ => throw NotKernelDimensions(kernelDimensions)
    }

    currentKernelType = Some(NDRangeKernel(globalRange, localRange))
    currentDimensions(LocalScope()) = localRange.indices.map(index => createRangeIterVar(LocalScope(), index, localRange(index))(localRange(index).o))
    currentDimensions(GroupScope()) = globalRange.indices.map(index =>
      createRangeIterVar(GroupScope(), index, FloorDiv(globalRange(index), localRange(index))(KernelRangeDivByZero(kernelDimensions))(localRange(index).o))(kernelDimensions.o))

    // Get the pre- and postcondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)
    val UnitAccountedPredicate(contractEnsures: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.ensures)

    implicit val o: Origin = kernelDeclaration.o

    // Create the parblock representing the work-groups
    val parBlock = ParBlock[Post](
      decl = new ParBlockDecl[Post]()(o.replacePrefName("SYCL_ND_RANGE_KERNEL")),
      iters = currentDimensions(GroupScope()) ++ currentDimensions(LocalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = contractRequires,
      ensures = contractEnsures,
      content = rw.dispatch(kernelDeclaration.body)
    )(KernelParFailure(kernelDeclaration))

    (parBlock, contractRequires, contractEnsures)
  }

  // Returns what kind of kernel we are working with and check that the kernel ranges match with the (nd_)item ranges
  private def getKernelRangeType(complicatedKernelRangeType: Type[Pre], lambdaDeclarator: CPPDeclarator[Pre]): Type[Pre] = {
    val kernelRangeType = complicatedKernelRangeType match {
      case primitive: CPPPrimitiveType[Pre] => CPP.getBaseTypeFromSpecs(primitive.specifiers)
      case typ => typ
    }
    val lambdaParams = CPP.paramsFromDeclarator(lambdaDeclarator)
    if (lambdaParams.size != 1) {
      throw IncorrectParallelForLambdaArgument(lambdaDeclarator.o)
    }
    val lambdaArgType = CPP.getBaseTypeFromSpecs(lambdaParams.head.specifiers)
    (kernelRangeType, lambdaArgType) match {
      case (SYCLTRange(rangeDimCount), SYCLTItem(itemDimCount)) if rangeDimCount == itemDimCount =>
      case (SYCLTNDRange(rangeDimCount), SYCLTNDItem(itemDimCount)) if rangeDimCount == itemDimCount =>
      case _ => throw IncorrectParallelForLambdaArgument(lambdaDeclarator.o)
    }
    kernelRangeType
  }

  // Generate the IterVariables that are passed to the parblock as ranges
  private def createRangeIterVar(scope: KernelScopeLevel, dimension: Int, maxRange: Expr[Post])(implicit o: Origin): IterVariable[Post] = {
    val variable = new Variable[Post](TInt())(o.replacePrefName(s"${scope.idName}_$dimension"))
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
      Seq((procedureRef.decl.contract.givenArgs.head.ref, LiteralSeq(TInt(), currentDimensions(level).map (iterVar => iterVar.variable.get))))

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
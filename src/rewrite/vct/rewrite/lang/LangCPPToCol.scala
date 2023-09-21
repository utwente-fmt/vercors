package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, SuccessionMap}
import vct.result.VerificationError.UserError

import scala.collection.immutable.Seq
import scala.collection.mutable

case object LangCPPToCol {

  sealed abstract class KernelScopeLevel(val idName: String)
  case class GlobalScope() extends KernelScopeLevel("GLOBAL_ID")
  case class LocalScope() extends KernelScopeLevel("LOCAL_ID")
  case class GroupScope() extends KernelScopeLevel("GROUP_ID")

  sealed abstract class KernelType(val dimCount: Int)
  case class BasicKernel(override val dimCount: Int) extends KernelType(dimCount)
  case class NDRangeKernel(override val dimCount: Int) extends KernelType(dimCount)

  case class WrongCPPType(decl: CPPLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongCPPType"

    override def text: String =
      decl.o.messageInContext(s"This declaration has a type that is not supported.")
  }

  case class CPPDoubleContracted(decl: CPPGlobalDeclaration[_], defn: CPPFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"

    override def text: String =
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }

  case class SYCLEventWaitWrongType(inv: CPPInvocation[_]) extends VerificationFailure {
    override def code: String = "unexpectedSYCLEventWaitWrongType"
    override def position: String = inv.o.shortPosition
    override def desc: String = inv.o.messageInContext("The declarator for the invocation to sycl::event::wait() has the wrong type.")
    override def inlineDesc: String = "The declarator for the invocation to sycl::event::wait() has the wrong type."
  }

  case class ContractForCommandGroup(contract: ApplicableContract[_]) extends UserError {
    override def code: String = "unsupportedContractForCommandGroup"

    override def text: String = contract.o.messageInContext(s"Contracts are not supported for SYCL command groups.")
  }

  case class GivenYieldsOnSYCLMethods(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "unsupportedGivenYieldsOnSYCLMethods"

    override def text: String = inv.o.messageInContext(s"Given and yields annotations are not supported for calls to built-in SYCL methods.")
  }

  case class MultipleKernels(decl1: Statement[_], decl2: CPPInvocation[_]) extends UserError {
    override def code: String = "multipleKernels"

    override def text: String = Origin.messagesInContext(Seq(
      decl2.o -> "This kernel declaration is not allowed, as only one kernel declaration is allowed per command group, ...",
      decl1.o -> "... and there is already a kernel declared here.",
    ))
  }

  case class MissingKernel(decl: Statement[_]) extends UserError {
    override def code: String = "missingKernel"

    override def text: String = decl.o.messageInContext(s"There is no kernel declaration in this command group.")
  }

  case class WrongKernelDimensionType(expr: Expr[_]) extends VerificationFailure {
    override def code: String = "wrongKernelDimensionType"
    override def position: String = expr.o.shortPosition
    override def desc: String = expr.o.messageInContext("Wrong type for the dimensions parameter of the kernel. " +
      "The dimensions parameter in a kernel declaration is supposed to be of type sycl::range<int> or sycl::nd_range<int>.")
    override def inlineDesc: String = "Wrong type for the dimensions parameter, it is supposed to be of type " +
      "sycl::range<int> or sycl::nd_range<int>."
  }

  case class NotKernelDimensions(expr: Expr[_]) extends VerificationFailure {
    override def code: String = "unexpectedNotKernelDimensions"
    override def position: String = expr.o.shortPosition
    override def desc: String = expr.o.messageInContext("The dimensions parameter of the kernel was not rewritten to a (nd_)range.")
    override def inlineDesc: String = "The dimensions parameter of the kernel was not rewritten to a (nd_)range."
  }

  case class KernelNotInjective(kernel: CPPLambdaDefinition[_]) extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      kernel.blame.blame(KernelPredicateNotInjective(Right(kernel), error.resource))
  }

  case class KernelRangeDivByZero(kernelDims: Expr[_]) extends Blame[DivByZero] {
    override def blame(error: DivByZero): Unit =
      PanicBlame(kernelDims.o.messageInContext("One or more dimensions in local range are zero, which is not allowed as the global range is divided by the local range to retrieve the group range.")).blame(error)
  }

  case class KernelParFailure(kernel: CPPLambdaDefinition[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
      case ParPredicateNotInjective(_, predicate) =>
        kernel.blame.blame(KernelPredicateNotInjective(Right(kernel), predicate))
      case ParPreconditionFailed(_, _) =>
        PanicBlame("Kernel parallel block precondition cannot fail, since an identical predicate is required before.").blame(error)
      case ParBlockPostconditionFailed(failure, _) =>
        kernel.blame.blame(KernelPostconditionFailed(failure, Right(kernel)))
      case ParBlockMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
    }
  }

  case class NoKernelIdAvailable(o: Origin) extends UserError {
    override def code: String = "noKernelIDAvailable"
    override def text: String = o.messageInContext("The requested kernel id was not available. This can happen if it is requested outside a kernel.")
  }

  case class TooHighKernelRangeDimension(o: Origin) extends UserError {
    override def code: String = "tooHighKernelRangeDimension"
    override def text: String = o.messageInContext("The kernel range contains less dimensions than the dimension requested here.")
  }

  case class IncorrectParallelForLambdaArgument(o: Origin) extends UserError {
    override def code: String = "incorrectParallelForLambdaArgument"
    override def text: String = o.messageInContext(f"Lambda parameters for the parallel_for method call only takes one (nd_)item argument should match the first (nd_)range parameter of the parallel_for method.")
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

  var currentDimensions: mutable.Map[KernelScopeLevel, Seq[IterVariable[Post]]] = mutable.Map.empty
  var currentKernelType: Option[KernelType] = None

  def rewriteUnit(cppUnit: CPPTranslationUnit[Pre]): Unit = {
    cppUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cppParam: CPPParam[Pre]): Unit = {
    cppParam.drop()
    val varO = InterpretedOriginVariable(CPP.getDeclaratorInfo(cppParam.declarator).name, cppParam.o)

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

    val namedO = InterpretedOriginVariable(info.name, func.o)
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
      // If name starts with sycl:: it can be removed, as all built-in SYCL methods are just used for resolution and type checking
      if (!info.name.startsWith("sycl::")) {
        val namedO = InterpretedOriginVariable(info.name, init.o)
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
    val varO: Origin = InterpretedOriginVariable(info.name, init.o)
    t match {
      case cta @ CPPTArray(Some(size), t) =>
        if (init.init.isDefined) throw WrongCPPType(decl)
        implicit val o: Origin = init.o
        val v = new Variable[Post](TArray(t))(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        val newArr = NewArray[Post](t, Seq(size), 0)(cta.blame)
        Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
      case typ =>
        var v = new Variable[Post](t)(varO)
        implicit val o: Origin = init.o
        if (init.init.isDefined) {
          val value = rw.dispatch(init.init.get)
          // Update the inhale variable if needed
          value match {
            case procInv: ProcedureInvocation[Post] if procInv.ref.decl.returnType.isInstanceOf[SYCLTEvent[Post]] =>
              // When method returns sycl::event, we need to get it from the method, as type of invocation does not have the correct inhale variable
              v = new Variable[Post](procInv.ref.decl.returnType.asInstanceOf[SYCLTEvent[Post]])(varO)
            case _ if typ.isInstanceOf[SYCLTEvent[Post]] =>
              v = new Variable[Post](value.t)(varO)
            case _ =>
          }
          cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
          Block(Seq(LocalDecl(v), assignLocal(v.get, value)))
        } else {
          cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
          LocalDecl(v)
        }
    }
  }

  def preAssign(preAssign: PreAssignExpression[Pre]): Expr[Post] = {
    val rewrittenPreAssign = rw.rewriteDefault(preAssign).asInstanceOf[PreAssignExpression[Post]]
    val postTarget = rewrittenPreAssign.target
    val postValue = rewrittenPreAssign.value
    (postTarget.t, postValue) match {
      // Update the inhale variable if needed
      case (targetEvent: SYCLTEvent[Post], procInv: ProcedureInvocation[Post]) if procInv.ref.decl.returnType.isInstanceOf[SYCLTEvent[Post]] =>
        // When method returns sycl::event, we need to get it from the method, as type of invocation does not have the correct inhale variable
        targetEvent.inhale = procInv.ref.decl.returnType.asInstanceOf[SYCLTEvent[Post]].inhale
      case (targetEvent: SYCLTEvent[Post], _) if postValue.t.isInstanceOf[SYCLTEvent[Post]] =>
        targetEvent.inhale = postValue.t.asInstanceOf[SYCLTEvent[Post]].inhale
      case _ =>
    }
    rewrittenPreAssign
  }

  def local(local: Either[CPPLocal[Pre], CPPClassInstanceLocal[Pre]]): Expr[Post] = {
    val (ref, blame, localExpr: Expr[Pre]) = local match {
      case Left(loc) => (loc.ref.get, loc.blame, loc)
      case Right(loc) => (loc.classLocalRef.get, loc.blame, loc)
    }
    implicit val o: Origin = localExpr.o
    ref match {
      case RefAxiomaticDataType(_) => throw NotAValue(localExpr)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(blame)
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

  def invocationStatement(eval: Eval[Pre]): Statement[Post] = {
    val inv = eval.expr.asInstanceOf[CPPInvocation[Pre]]
    if (inv.ref.get.name == "sycl::event::wait" && inv.applicable.isInstanceOf[CPPClassInstanceLocal[Pre]]) {
      implicit val o: Origin = inv.o
      val classRef = inv.applicable.asInstanceOf[CPPClassInstanceLocal[Pre]].classInstanceRef.get
      cppNameSuccessor.get(classRef) match {
        case Some(v: Variable[Post]) if v.t.isInstanceOf[SYCLTEvent[Post]] =>
          val returnVal = v.t.asInstanceOf[SYCLTEvent[Post]].inhale.getOrElse(Block(Seq()))
          // Remove inhale value, so you cannot 'wait' twice on the same kernel
          v.t.asInstanceOf[SYCLTEvent[Post]].inhale = None
          returnVal
        case _ => throw BlameUnreachable("Type is not SYCLTEvent.", SYCLEventWaitWrongType(inv))
      }
    } else {
      rw.rewriteDefault(eval)
    }
  }

  def invocation(inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefFunction(decl) =>
        FunctionInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefProcedure(decl) =>
        ProcedureInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefPredicate(decl) =>
        PredicateApply[Post](rw.succ(decl), args.map(rw.dispatch), WritePerm())
      case RefInstanceFunction(decl) => ???
      case RefInstanceMethod(decl) => ???
      case RefInstancePredicate(decl) => ???
      case RefADTFunction(decl) =>
        ADTFunctionInvocation[Post](None, rw.succ(decl), args.map(rw.dispatch))
      case RefModelProcess(decl) =>
        ProcessApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case RefModelAction(decl) =>
        ActionApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case BuiltinInstanceMethod(f) => ???
      case ref: RefCPPFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](cppFunctionSuccessor.ref(ref.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefCPPLambdaDefinition(_) => ???
      case e: RefCPPGlobalDeclaration[Pre] => globalInvocation(e, inv)
      case RefProverFunction(decl) => ProverFunctionInvocation(rw.succ(decl), args.map(rw.dispatch))
    }
  }

  def globalInvocation(e: RefCPPGlobalDeclaration[Pre], inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    val firstArgIntValue = if (args.size == 1) {
      args.head match {
        case IntegerValue(i) => Some(i.toInt)
        case _ => None
      }
    } else None
    (e.name, firstArgIntValue) match {
      case ("sycl::range::constructor", _) => SYCLRange[Post](args.map(rw.dispatch))
      case ("sycl::nd_range::constructor", None) => SYCLNDRange[Post](rw.dispatch(args.head), rw.dispatch(args(1)))
      case ("sycl::item::get_id", Some(i)) => getSYCLWorkItemId(GlobalScope(), i)
      case ("sycl::item::get_linear_id", None) => getSYCLLinearWorkItemId(GlobalScope())
      case ("sycl::item::get_range", Some(i)) => getSYCLWorkItemRange(GlobalScope(), i)
      case ("sycl::nd_item::get_global_id", Some(i)) => getSYCLWorkItemId(GlobalScope(), i)
      case ("sycl::nd_item::get_global_linear_id", None) => getSYCLLinearWorkItemId(GlobalScope())
      case ("sycl::nd_item::get_global_range", Some(i)) => getSYCLWorkItemRange(GlobalScope(), i)
      case ("sycl::nd_item::get_local_id", Some(i)) => getSYCLWorkItemId(LocalScope(), i)
      case ("sycl::nd_item::get_local_linear_id", None) => getSYCLLinearWorkItemId(GlobalScope())
      case ("sycl::nd_item::get_local_range", Some(i)) => getSYCLWorkItemRange(LocalScope(), i)
      case ("sycl::nd_item::get_group_id", Some(i)) => getSYCLWorkItemId(GroupScope(), i)
      case ("sycl::nd_item::get_group_linear_id", None) => getSYCLLinearWorkItemId(GlobalScope())
      case ("sycl::nd_item::get_group_range", Some(i)) => getSYCLWorkItemRange(GroupScope(), i)
      case ("sycl::queue::submit", None) => rewriteSYCLQueueSubmit(inv)
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

  private def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): Expr[Post] = {
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
    } else if (collectedKernelDeclarations.head.givenArgs.nonEmpty || collectedKernelDeclarations.head.yields.nonEmpty) {
      // Do not allow given and yields on the kernel
      throw GivenYieldsOnSYCLMethods(collectedKernelDeclarations.head)
    }

    val kernelDimensions = collectedKernelDeclarations.head.args.head
    val kernelDeclaration = collectedKernelDeclarations.head.args(1).asInstanceOf[CPPLambdaDefinition[Pre]]

    // Get the kernel range type (which also checks if it is valid)
    val rangeType = getKernelRangeType(kernelDimensions.t, kernelDeclaration.declarator)

    // Create a block of code for the kernel body based on what type of kernel it is
    val kernelParBlock = rangeType match {
      case SYCLTRange(_) => createBasicKernelBody(kernelDimensions, kernelDeclaration)
      case SYCLTNDRange(_) => createNDRangeKernelBody(kernelDimensions, kernelDeclaration)
      case _ => throw BlameUnreachable("Type mismatch", WrongKernelDimensionType(kernelDimensions))
    }
    val kernelBody = ParStatement[Post](kernelParBlock)(kernelDeclaration.body.o)

    // Create the precondition for the method that will hold the generated kernel code
    val preCondition = getKernelQuantifiedCondition(rw.dispatch(kernelDeclaration.contract.requires), KernelNotInjective(kernelDeclaration))
    // Create the post condition that will be inhaled when wait() is called on this kernel's event
    val postCondition = getKernelQuantifiedCondition(rw.dispatch(kernelDeclaration.contract.ensures), KernelNotInjective(kernelDeclaration))

    // Create a contract for the method that will hold the generated kernel code
    val methodContract = {
      implicit val o: Origin = commandGroupBody.o
      ApplicableContract[Post](
        UnitAccountedPredicate(preCondition)(kernelDeclaration.contract.requires.o),
        UnitAccountedPredicate(AstBuildHelpers.foldStar(Seq())),
        AstBuildHelpers.foldStar(Seq()),
        Seq(), Seq(), Seq(), None
      )(kernelDeclaration.contract.blame)
    }

    // Declare the newly generated kernel code inside a new method
    val proc = rw.globalDeclarations.declare(
      {
        rw.labelDecls.scope {
          new Procedure[Post](
            returnType = SYCLTEvent[Post](Some(Inhale[Post](postCondition)(invocation.o)))(invocation.o),
            args = Seq(),
            outArgs = Nil,
            typeArgs = Nil,
            body = Some(kernelBody),
            contract = methodContract,
          )(kernelDeclaration.blame)(InterpretedOriginVariable("VERCORS_SYCL_KERNEL_WRAPPER", kernelDeclaration.o))
        }
      }
    )

    // Return an invocation to the new method holding the kernel code
    currentKernelType = None
    ProcedureInvocation[Post](new DirectRef(proc), Seq(), Nil, Nil, Seq(), Seq())(invocation.blame)(invocation.o)
  }

  private def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): ParBlock[Post] = {
    implicit val o: Origin = kernelDeclaration.body.o

    // Register the kernel dimensions
    val range: Seq[Expr[Post]] = rw.dispatch(kernelDimensions) match {
      case SYCLRange(dims) => dims
      case _ => throw BlameUnreachable("Not kernel dimensions", NotKernelDimensions(kernelDimensions))
    }

    currentKernelType = Some(BasicKernel(range.size))
    currentDimensions(GlobalScope()) = range.indices.map(index => createRangeIterVar(GlobalScope(), index, range(index)))

    // Get the precondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)

    // Create the parblock representing the kernels
    ParBlock[Post](
      decl = new ParBlockDecl[Post]()(SourceNameOrigin("SYCL_BASIC_KERNEL", o)),
      iters = currentDimensions(GlobalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = contractRequires,
      ensures = BooleanValue(value = true), // Do not add ensures contract here, is instead inhaled when calling event::wait()
      content = rw.dispatch(kernelDeclaration.body),
    )(KernelParFailure(kernelDeclaration))
  }

  private def createNDRangeKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): ParBlock[Post] = {
    implicit val o: Origin = kernelDeclaration.body.o

    // Register the kernel dimensions
    val (globalRange, localRange): (Seq[Expr[Post]], Seq[Expr[Post]]) = rw.dispatch(kernelDimensions) match {
      case SYCLNDRange(globalSize: SYCLRange[Post], localRange: SYCLRange[Post]) => (globalSize.dimensions, localRange.dimensions)
      case _ => throw BlameUnreachable("Not kernel dimensions", NotKernelDimensions(kernelDimensions))
    }

    currentKernelType = Some(NDRangeKernel(globalRange.size))
    currentDimensions(LocalScope()) = localRange.indices.map(index => createRangeIterVar(LocalScope(), index, localRange(index)))
    // Cannot check whether global range is a multiple of local range, so this is the responsibility of the user
    currentDimensions(GroupScope()) = globalRange.indices.map(index =>
      createRangeIterVar(GroupScope(), index,  FloorDiv(globalRange(index), localRange(index))(KernelRangeDivByZero(kernelDimensions))))

    // Get the precondition
    val UnitAccountedPredicate(contractRequires: Expr[Post]) = rw.dispatch(kernelDeclaration.contract.requires)

    // Create the parblock representing the work-groups
    ParBlock[Post](
      decl = new ParBlockDecl[Post]()(SourceNameOrigin("SYCL_ND_RANGE_KERNEL", o)),
      iters = currentDimensions(GroupScope()) ++ currentDimensions(LocalScope()),
      context_everywhere = rw.dispatch(kernelDeclaration.contract.contextEverywhere),
      requires = contractRequires,
      ensures = BooleanValue(value = true), // Do not add ensures contract here, is instead inhaled when calling event::wait()
      content = rw.dispatch(kernelDeclaration.body)
    )(KernelParFailure(kernelDeclaration))
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

  // Used for generation the contract for the method wrapping the parblocks and for the postcondition that is inhaled when event::wait() is called
  private def getKernelQuantifiedCondition(condition: AccountedPredicate[Post], blame: KernelNotInjective): Starall[Post] = {
    implicit val o: Origin = condition.o

    val UnitAccountedPredicate(contractCondition: Expr[Post]) = condition
    val (names, bindings) = currentDimensions.values.toSeq.flatten.foldLeft((Seq(): Seq[Variable[Post]], Seq(): Seq[SeqMember[Post]]))((acc, iterVar: IterVariable[Post]) => {
      (acc._1 :+ iterVar.variable, acc._2 :+ SeqMember(Local(iterVar.variable.ref), Range(iterVar.from, iterVar.to)))
    })
    Starall(names, Nil, Implies(foldAnd(bindings), contractCondition))(blame)
  }

  // Generates COL code that returns the id of a work-item
  private def getSYCLWorkItemId(level: KernelScopeLevel, index: Int)(implicit o: Origin): Expr[Post] = {
    if (currentKernelType.isEmpty) {
      throw NoKernelIdAvailable(o)
    }
    if (index >= currentKernelType.get.dimCount) {
      throw TooHighKernelRangeDimension(o)
    }
    (currentKernelType.get, level) match {
      case (BasicKernel(_), GlobalScope()) |
           (NDRangeKernel(_), LocalScope()) |
           (NDRangeKernel(_), GroupScope()) =>
        new Local[Post](new DirectRef(currentDimensions(level)(index).variable))
      case (NDRangeKernel(_), GlobalScope()) =>
        Plus(
          getSYCLWorkItemId(GroupScope(), index),
          Mult(getSYCLWorkItemId(LocalScope(), index), getSYCLWorkItemRange(GroupScope(), index))
        )
      case _ => throw NoKernelIdAvailable(o)
    }
  }

  // Generates COL code that returns the linear id of a work-item
  private def getSYCLLinearWorkItemId(level: KernelScopeLevel)(implicit o: Origin): Expr[Post] = {
    if (currentKernelType.isEmpty) {
      throw NoKernelIdAvailable(o)
    }
    // See SYCL Specification section 3.11.1 for the linearization formulas
    currentKernelType.get.dimCount match {
      case 1 => getSYCLWorkItemId(level, 0)
      case 2 =>
        val xRef = getSYCLWorkItemId(level, 0)
        val yRef = getSYCLWorkItemId(level, 1)
        val yRange = getSYCLWorkItemRange(level, 1)
        Plus(yRef, Mult(xRef, yRange))
      case 3 =>
        val xRef = getSYCLWorkItemId(level, 0)
        val yRef = getSYCLWorkItemId(level, 1)
        val zRef = getSYCLWorkItemId(level, 2)
        val yRange = getSYCLWorkItemRange(level, 1)
        val zRange = getSYCLWorkItemRange(level, 2)
        Plus(Plus(zRef, Mult(yRef, zRange)), Mult(Mult(xRef, yRange), zRange))
    }
  }

  // Generates COL code that returns the range of a work-item
  private def getSYCLWorkItemRange(level: KernelScopeLevel, index: Int)(implicit o: Origin): Expr[Post] = {
    if (currentKernelType.isEmpty) {
      throw NoKernelIdAvailable(o)
    }
    if (index >= currentKernelType.get.dimCount) {
      throw TooHighKernelRangeDimension(o)
    }
    (currentKernelType.get, level) match {
      case (BasicKernel(_), GlobalScope()) |
           (NDRangeKernel(_), LocalScope()) |
           (NDRangeKernel(_), GroupScope()) => currentDimensions(level)(index).to
      case (NDRangeKernel(_), GlobalScope()) =>
        Mult(getSYCLWorkItemRange(LocalScope(), index), getSYCLWorkItemRange(GroupScope(), index))
      case _ => throw NoKernelIdAvailable(o)
    }
  }

  // Generate the IterVariables that are passed to the parblock as ranges
  private def createRangeIterVar(scope: KernelScopeLevel, dimension: Int, maxRange: Expr[Post])(implicit o: Origin): IterVariable[Post] = {
    val variable = new Variable[Post](TInt())(SourceNameOrigin(s"${scope.idName}_${dimension}", o))
    new IterVariable[Post](variable, IntegerValue(0), maxRange)
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

  def syclEventType(t: SYCLTEvent[Pre]): Type[Post] = {
    SYCLTEvent[Post](t.inhale.map(rw.dispatch))(t.o)
  }

}
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

  case class ContractForCommandGroup(contract: ApplicableContract[_]) extends UserError {
    override def code: String = "unsupportedContractForCommandGroup"

    override def text: String = contract.o.messageInContext(s"Contracts are not supported for SYCL command groups.")
  }

  case class GivenYieldsOnSYCLMethods(inv: CPPInvocation[_]) extends UserError {
    override def code: String = "unsupportedGivenYieldsOnSYCLMethods"

    override def text: String = inv.o.messageInContext(s"Given and yields annotations are not supported for calls to built-in SYCL methods.")
  }

  case class MultipleKernels(decl1: Statement[_], decl2: CPPInvocation[_]) extends UserError {
    override def code: String = "unsupportedMultipleKernels"

    override def text: String = Origin.messagesInContext(Seq(
      decl2.o -> "This kernel declaration is not allowed, as only one kernel declaration is allowed per command group, ...",
      decl1.o -> "... and there is already a kernel declared here.",
    ))
  }

  case class MissingKernel(decl: Statement[_]) extends UserError {
    override def code: String = "unsupportedMissingKernel"

    override def text: String = decl.o.messageInContext(s"There is no kernel declaration in this command group.")
  }

  case class WrongKernelDimensionType(expr: Expr[_]) extends VerificationFailure {
    override def code: String = "unexpectedKernelDimensionType"
    override def position: String = expr.o.shortPosition
    override def desc: String = expr.o.messageInContext("Wrong type for the dimensions parameter of the kernel. " +
      "The dimensions parameter in a kernel declaration is supposed to be of type sycl::range<int> or sycl::nd_range<int>.")
    override def inlineDesc: String = "Wrong type for the dimensions parameter, it is supposed to be of type " +
      "sycl::range<int> or sycl::nd_range<int>."
  }

  case class NotKernelDimensions(expr: Expr[_]) extends VerificationFailure {
    override def code: String = "unexpectedNotKernelDimensions"
    override def position: String = expr.o.shortPosition
    override def desc: String = expr.o.messageInContext("The dimensions parameter of the kernel was not rewritten into a sequence of integers.")
    override def inlineDesc: String = "The dimensions parameter of the kernel was not rewritten into a sequence of integers."
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
    override def code: String = "unexpectedNoKernelIDAvailable"
    override def text: String = o.messageInContext("The requested kernel id was not available. This can happen if it is requested outside a kernel.")
  }

  case class TooHighKernelRangeDimension(o: Origin) extends UserError {
    override def code: String = "unexpectedTooHighKernelRangeDimension"

    override def text: String = o.messageInContext("The kernel range contains less dimensions that requested here.")
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

  var currentDimensions: mutable.Map[KernelScopeLevel, Seq[IterVariable[Pre]]] = mutable.Map.empty
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
    val infoParams = CPP.filterOutLambdaParams(info.params.get)
    val returnType = func.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get

    val (contract, subs: Map[CPPParam[Pre], CPPParam[Pre]]) = func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) if decl.decl.contract.nonEmpty =>
        if (func.contract.nonEmpty) throw CPPDoubleContracted(decl, func)
        // EW TODO: params are also filtered here, is ok?
        val declParams = CPP.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
        val defnParams = infoParams

        (decl.decl.contract, declParams.zip(defnParams).toMap)
      case _ =>
        (func.contract, Map.empty)
    }

    val namedO = InterpretedOriginVariable(CPP.getDeclaratorInfo(func.declarator).name, func.o)
    val proc =
      cppCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
          {
            val params = rw.variables.collect {
              infoParams.foreach(rw.dispatch)
            }._1
            rw.labelDecls.scope {
              new Procedure[Post](
                returnType = returnType,
                args = params,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.dispatch(func.body)),
                contract = rw.dispatch(contract),
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
      val namedO = InterpretedOriginVariable(info.name, init.o)
      info.params match {
        case Some(params) =>
          cppFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            new Procedure[Post](
              returnType = t,
              args = rw.variables.collect {
                CPP.filterOutLambdaParams(params).foreach(rw.dispatch)
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
          value match {
            // get type from value as it contains the updated inhale variable
            case procInv: ProcedureInvocation[Post] if procInv.ref.decl.returnType.isInstanceOf[SYCLTEvent[Post]] =>
              v = new Variable[Post](procInv.ref.decl.returnType.asInstanceOf[SYCLTEvent[Post]])(varO)
            case _ if typ.isInstanceOf[SYCLTEvent[Post]] => {
              v = new Variable[Post](value.t)(varO)
            }
            case _ =>
          }
          cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
          Block(Seq(LocalDecl(v), assignLocal(v.get, value)))
        } else {
          LocalDecl(new Variable[Post](t)(varO))
        }
    }
  }

  def preAssign(preAssign: PreAssignExpression[Pre]): Expr[Post] = {
    val postTarget = rw.dispatch(preAssign.target)
    val postValue = rw.dispatch(preAssign.value)
    (postTarget.t, postValue) match {
      // get type from value as it contains the updated inhale variable
      case (targetEvent: SYCLTEvent[Post], procInv: ProcedureInvocation[Post]) if procInv.ref.decl.returnType.isInstanceOf[SYCLTEvent[Post]] =>
        targetEvent.inhale = procInv.ref.decl.returnType.asInstanceOf[SYCLTEvent[Post]].inhale
      case (targetEvent: SYCLTEvent[Post], _) if postValue.t.isInstanceOf[SYCLTEvent[Post]] =>
        targetEvent.inhale =  postValue.t.asInstanceOf[SYCLTEvent[Post]].inhale
      case _ =>
    }
    PreAssignExpression(postTarget, postValue)(preAssign.blame)(preAssign.o)
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

  def classInstanceLocal(classInstanceLocal: CPPClassInstanceLocal[Pre]): Expr[Post] = {
    local(Right(classInstanceLocal))
  }

  def invocationStatement(eval: Eval[Pre]): Statement[Post] = {
    val inv = eval.expr.asInstanceOf[CPPInvocation[Pre]]
    if (inv.ref.get.name == "sycl::event::wait") {
      implicit val o: Origin = inv.o
      val classRef = inv.applicable.asInstanceOf[CPPClassInstanceLocal[Pre]].classInstanceRef.get
      cppNameSuccessor.get(classRef) match {
        case Some(v: Variable[Post]) if v.t.isInstanceOf[SYCLTEvent[Post]] =>
          // remove inhale, so you cannot 'wait' twice on the same kernel
          val returnVal = v.t.asInstanceOf[SYCLTEvent[Post]].inhale.getOrElse(Block(Seq()))
          v.t.asInstanceOf[SYCLTEvent[Post]].inhale = None
          returnVal
        case _ => Block(Seq()) // EW TODO: throw error
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
        var filteredArgs = Seq[Expr[Pre]]()
        args.foreach {
          case _: CPPLambdaDefinition[Pre] =>
          case argument => filteredArgs = filteredArgs :+ argument
        }
        ProcedureInvocation[Post](cppFunctionSuccessor.ref(ref.decl), filteredArgs.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case _: RefCPPLambdaDefinition[Pre] => ???
      case e: RefCPPGlobalDeclaration[Pre] => globalInvocation(e, inv)
      case RefProverFunction(decl) => ProverFunctionInvocation(rw.succ(decl), args.map(rw.dispatch))
    }
  }

  def globalInvocation(e: RefCPPGlobalDeclaration[Pre], inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    // Process lambda parameters and remove them from the parameter list
    var filteredArgs = Seq[Expr[Pre]]()
    args.foreach {
      case argument: CPPLambdaDefinition[Pre] =>
      case argument => filteredArgs = filteredArgs :+ argument
    }
    val firstArgIntValue = if (filteredArgs.size == 1) {
      filteredArgs.head match {
        case IntegerValue(i) if i >= 0 && i < 3 => Some(i.toInt)
        case _ => None
      }
    } else None
    (e.name, firstArgIntValue) match {
      case ("sycl::range::constructor", _) => {
        SYCLRange[Post](filteredArgs.map {
          case IntegerValue(i) => i.toInt
          case _ => -1
        })
      }
      case ("sycl::nd_range::constructor", _) =>
        SYCLNDRange[Post](rw.dispatch(filteredArgs.head), rw.dispatch(filteredArgs(1)))
      case ("sycl::item::get_id", Some(i)) => rw.dispatch(getSYCLWorkItemId(GlobalScope(), i))
      case ("sycl::item::get_linear_id", None) => rw.dispatch(getSYCLLinearWorkItemId(GlobalScope()))
      case ("sycl::item::get_range", Some(i)) => rw.dispatch(getSYCLWorkItemRange(GlobalScope(), i))
      case ("sycl::nd_item::get_global_id", Some(i)) => rw.dispatch(getSYCLWorkItemId(GlobalScope(), i))
      case ("sycl::nd_item::get_global_linear_id", None) => rw.dispatch(getSYCLLinearWorkItemId(GlobalScope()))
      case ("sycl::nd_item::get_global_range", Some(i)) => rw.dispatch(getSYCLWorkItemRange(GlobalScope(), i))
      case ("sycl::nd_item::get_local_id", Some(i)) => rw.dispatch(getSYCLWorkItemId(LocalScope(), i))
      case ("sycl::nd_item::get_local_linear_id", None) => rw.dispatch(getSYCLLinearWorkItemId(GlobalScope()))
      case ("sycl::nd_item::get_local_range", Some(i)) => rw.dispatch(getSYCLWorkItemRange(LocalScope(), i))
      case ("sycl::nd_item::get_group_id", Some(i)) => rw.dispatch(getSYCLWorkItemId(GroupScope(), i))
      case ("sycl::nd_item::get_group_linear_id", None) => rw.dispatch(getSYCLLinearWorkItemId(GlobalScope()))
      case ("sycl::nd_item::get_group_range", Some(i)) => rw.dispatch(getSYCLWorkItemRange(GroupScope(), i))
      case ("sycl::queue::submit", _) => rewriteSYCLQueueSubmit(inv)
      case _ => {
        val procedureRef: Ref[Post, Procedure[Post]] = cppFunctionDeclSuccessor.ref((decls, initIdx))
        ProcedureInvocation[Post](
          procedureRef, filteredArgs.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) }
        )(inv.blame)
      }
    }
  }


  def getSYCLWorkItemId(level: KernelScopeLevel, index: Int)(implicit o: Origin): Expr[Pre] = {
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
        new Local[Pre](new DirectRef(currentDimensions(level)(index).variable))
      case (NDRangeKernel(_), GlobalScope()) =>
        Plus(
          getSYCLWorkItemId(GroupScope(), index),
          Mult(getSYCLWorkItemId(LocalScope(), index), getSYCLWorkItemRange(GroupScope(), index))
        )
      case _ => throw NoKernelIdAvailable(o)
    }
  }

  def getSYCLWorkItemRange(level: KernelScopeLevel, index: Int)(implicit o: Origin): Expr[Pre] = {
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

  def getSYCLLinearWorkItemId(level: KernelScopeLevel)(implicit o: Origin): Expr[Pre] = {
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

  def createRangeIterVar(scope: KernelScopeLevel, dimension: Int, maxRange: Int)(implicit o: Origin): IterVariable[Pre] = {
    val variable = new Variable[Pre](TInt())(SourceNameOrigin(s"${scope.idName}_${dimension}", o))
    new IterVariable[Pre](variable, IntegerValue(0), IntegerValue(maxRange))
  }

  def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): Expr[Post] = {
    // Do not allow given and yields on the invocation
    if (invocation.givenArgs.nonEmpty || invocation.yields.nonEmpty) throw GivenYieldsOnSYCLMethods(invocation)

    // Get the lambda describing the command group
    val commandGroup = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]]
    val commandGroupBody: Statement[Pre] = commandGroup.body

    // Do not allow contract for the command group
    if (commandGroup.contract.nonEmpty) throw ContractForCommandGroup(commandGroup.contract)


    // Get the kernel declarations in the command group
    val collectedKernelDeclarations: Seq[CPPInvocation[Pre]] = {
      commandGroupBody.asInstanceOf[Scope[Pre]].body.asInstanceOf[Block[Pre]].statements.collect({
        case Eval(inv) if inv.isInstanceOf[CPPInvocation[Pre]] && inv.asInstanceOf[CPPInvocation[Pre]].ref.isDefined &&
          inv.asInstanceOf[CPPInvocation[Pre]].ref.get.name == "sycl::handler::parallel_for" =>
          inv.asInstanceOf[CPPInvocation[Pre]]
      })
    }
    // Make sure there is only one kernel definition
    if (collectedKernelDeclarations.isEmpty){
      throw MissingKernel(commandGroupBody)
    } else if (collectedKernelDeclarations.size > 1) {
      throw MultipleKernels(commandGroupBody, collectedKernelDeclarations(1))
    } else if (collectedKernelDeclarations.head.givenArgs.nonEmpty || collectedKernelDeclarations.head.yields.nonEmpty) {
      // Do not allow given and yields on the invocation
      throw GivenYieldsOnSYCLMethods(collectedKernelDeclarations.head)
    }

    val kernelDimensions = collectedKernelDeclarations.head.args.head
    val kernelDeclaration = collectedKernelDeclarations.head.args(1).asInstanceOf[CPPLambdaDefinition[Pre]]

    // Create a block of code based on what kind of kernel it is
    val kernelBody = kernelDimensions.t match {
      case primitive: CPPPrimitiveType[_] if CPP.getPrimitiveType(primitive.specifiers).isInstanceOf[SYCLTRange[_]] =>
        createBasicKernelBody(kernelDimensions, kernelDeclaration)
      case SYCLTRange(_) => createBasicKernelBody(kernelDimensions, kernelDeclaration)
      case primitive: CPPPrimitiveType[_] if CPP.getPrimitiveType(primitive.specifiers).isInstanceOf[SYCLTNDRange[_]] =>
        createNDRangeKernelBody(kernelDimensions, kernelDeclaration)
      case SYCLTNDRange(_) => createNDRangeKernelBody(kernelDimensions, kernelDeclaration)
      case _ => throw BlameUnreachable("Type mismatch", WrongKernelDimensionType(kernelDimensions))
    }

    // EW: Contract will be replaced with permissions for data accessors
    // Create a contract for the method that will hold the generated kernel code
    val methodContract = {
      implicit val o: Origin = commandGroupBody.o
      ApplicableContract[Pre](
        UnitAccountedPredicate(AstBuildHelpers.foldStar(Seq())),
        UnitAccountedPredicate(AstBuildHelpers.foldStar(Seq())),
        AstBuildHelpers.foldStar(Seq()),
        Seq(), Seq(), Seq(), None
      )(kernelDeclaration.contract.blame)
    }

    // Create the postCondition that will be inhaled when wait() is called on this kernel's event
    val postCondition = {
      implicit val o: Origin = kernelDeclaration.contract.ensures.o

      val UnitAccountedPredicate(contractEnsures: Expr[Pre]) = kernelDeclaration.contract.ensures
      var names: Seq[Variable[Pre]] = Seq()
      var bindings: Seq[SeqMember[Pre]] = Seq()
      currentDimensions.values.toSeq.flatten.foreach(iterVar => {
        names = names :+ iterVar.variable
        bindings = bindings :+ SeqMember(Local(iterVar.variable.ref), Range(iterVar.from, iterVar.to))
      })
      Forall(names, Nil, Implies(foldAnd(bindings), contractEnsures))
    }

    // Declare the newly generated kernel code inside a new method
    val proc = rw.globalDeclarations.declare(
      {
        rw.labelDecls.scope {
          new Procedure[Post](
            returnType = SYCLTEvent[Post](Some(Inhale[Post](rw.dispatch(postCondition))(invocation.o)))(invocation.o),
            args = Seq(),
            outArgs = Nil,
            typeArgs = Nil,
            body = Some(rw.dispatch(kernelBody)),
            contract = rw.dispatch(methodContract),
          )(kernelDeclaration.blame)(InterpretedOriginVariable("VERCORS_SYCL_KERNEL_WRAPPER", kernelDeclaration.o))
        }
      }
    )

    currentKernelType = None

    // Return an invocation to the new method holding the kernel code
    ProcedureInvocation[Post](new DirectRef(proc), Seq(), Nil, Nil, Seq(), Seq())(invocation.blame)(invocation.o)
  }

  def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): ParStatement[Pre] = {
    implicit val o: Origin = kernelDeclaration.body.o

    // Register the kernel dimensions
    val range: Seq[Int] = rw.dispatch(kernelDimensions) match {
      case SYCLRange(dims) => dims
      case _ => throw BlameUnreachable("Not kernel dimensions", NotKernelDimensions(kernelDimensions))
    }

    currentKernelType = Some(BasicKernel(range.size))
    currentDimensions(GlobalScope()) = range.indices.map(index => createRangeIterVar(GlobalScope(), index, range(index)))

    // Set the pre- and post-conditions
    val UnitAccountedPredicate(contractRequires: Expr[Pre]) = kernelDeclaration.contract.requires

    // Create the parblock representing the kernels
    ParStatement[Pre](ParBlock[Pre](
      decl = new ParBlockDecl[Pre]()(SourceNameOrigin("SYCL_BASIC_KERNEL", o)),
      iters = currentDimensions(GlobalScope()),
      context_everywhere = kernelDeclaration.contract.contextEverywhere,
      requires = contractRequires,
      ensures = BooleanValue(value = true), // Do not add ensures contract here, is instead inhaled when calling event::wait()
      content = kernelDeclaration.body,
    )(KernelParFailure(kernelDeclaration)))
  }

  def createNDRangeKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): ParStatement[Pre] = {
    implicit val o: Origin = kernelDeclaration.body.o

    // Register the kernel dimensions
    val (globalRange, localRange): (Seq[Int], Seq[Int]) = rw.dispatch(kernelDimensions) match {
      case SYCLNDRange(globalSize: SYCLRange[Post], localRange: SYCLRange[Post]) => (globalSize.dimensions, localRange.dimensions)
      case _ => throw BlameUnreachable("Not kernel dimensions", NotKernelDimensions(kernelDimensions))
    }
    currentKernelType = Some(NDRangeKernel(globalRange.size))
    currentDimensions(LocalScope()) = localRange.indices.map(index => createRangeIterVar(LocalScope(), index, localRange(index)))
    currentDimensions(GroupScope()) = globalRange.indices.map(index => createRangeIterVar(GroupScope(), index, globalRange(index) / localRange(index)))

    // Set the pre- and post-conditions
    val UnitAccountedPredicate(contractRequires: Expr[Pre]) = kernelDeclaration.contract.requires

    // Create the parblock representing the work-groups
    ParStatement[Pre](ParBlock[Pre](
      decl = new ParBlockDecl[Pre]()(SourceNameOrigin("SYCL_ND_RANGE_KERNEL", o)),
      iters = currentDimensions(GroupScope()) ++ currentDimensions(LocalScope()),
      context_everywhere = kernelDeclaration.contract.contextEverywhere,
      requires = contractRequires,
      ensures = BooleanValue(value = true), // Do not add ensures contract here, is instead inhaled when calling event::wait()
      content = kernelDeclaration.body
    )(KernelParFailure(kernelDeclaration)))
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
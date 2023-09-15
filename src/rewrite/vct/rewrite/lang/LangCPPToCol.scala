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

case object LangCPPToCol {

  sealed abstract class KernelScopeLevel(val idName: String)
  case class GlobalScope() extends KernelScopeLevel("GLOBAL_ID");
  case class LocalScope() extends KernelScopeLevel("LOCAL_ID");
  case class GroupScope() extends KernelScopeLevel("GROUP_ID");

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

  case class KernelParFailure(kernel: CPPLambdaDefinition[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
      case _ => PanicBlame("ELLEN: Implement blame!").blame(error)
    }
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

  case class WrongKernelDimensionsType(expr: Expr[_]) extends VerificationFailure {
    override def code: String = "unexpectedKernelDimensionType"
    override def position: String = expr.o.shortPosition
    override def desc: String = expr.o.messageInContext("Wrong type for the dimensions parameter of the kernel. The dimensions parameter in a kernel declaration is supposed to be of type sycl::range<int> or sycl::nd_range<int>.")
    override def inlineDesc: String = "Wrong type for the dimensions parameter, it is supposed to be of type sycl::range<int> or sycl::nd_range<int>."
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
  val cppLambdaSuccessor: SuccessionMap[CPPLambdaDefinition[Pre], Procedure[Post]] = SuccessionMap()

  var currentGlobalDimensions: ScopedStack[Seq[IterVariable[Pre]]] = ScopedStack()
  var currentLocalDimensions: ScopedStack[Seq[IterVariable[Pre]]] = ScopedStack()
  var currentGroupDimensions: ScopedStack[Seq[IterVariable[Pre]]] = ScopedStack()

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

  def rewriteLocal(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
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
      case _ =>
        val v = new Variable[Post](t)(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        implicit val o: Origin = init.o
        init.init
          .map(value => Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value)))))
          .getOrElse(LocalDecl(v))
    }
  }

  def result(ref: RefCPPFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cppFunctionSuccessor.ref(ref.decl))

  def result(ref: RefCPPLambdaDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cppLambdaSuccessor.ref(ref.decl))

  def result(ref: RefCPPGlobalDeclaration[Pre])(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cppFunctionSuccessor.ref(defn.decl))
      case None => Result[Post](cppFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def local(local: CPPLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case RefAxiomaticDataType(_) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCPPParam[Pre] =>
        if (cppCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cppNameSuccessor.ref(RefCPPParam(cppCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cppNameSuccessor.ref(ref))
      case RefCPPFunctionDefinition(_) => throw NotAValue(local)
      case ref @ RefCPPGlobalDeclaration(decl, initIdx) =>
        CPP.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None => DerefHeapVariable[Post](cppGlobalNameSuccessor.ref(ref))(local.blame)
          case Some(_) => throw NotAValue(local)
        }
      case ref: RefCPPLocalDeclaration[Pre] => Local(cppNameSuccessor.ref(ref))
    }
  }

  def invocation(inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(applicable, args, givenMap, yields) = inv
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
          case argument: CPPLambdaDefinition[Pre] =>
          case argument => filteredArgs = filteredArgs :+ argument
        }
        ProcedureInvocation[Post](cppFunctionSuccessor.ref(ref.decl), filteredArgs.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case ref: RefCPPLambdaDefinition[Pre] =>
        ProcedureInvocation[Post](cppLambdaSuccessor.ref(ref.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
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
    }
    else None
    (e.name, firstArgIntValue) match {
      case ("sycl::range::constructor", _) => {
        SYCLRange[Post](filteredArgs.map {
          case IntegerValue(i) => i.toInt
          case _ => -1
        })
      }
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

  def getRangeDimensions(level: KernelScopeLevel): Seq[IterVariable[Pre]] =
    try {
      level match {
        case GlobalScope() => currentGlobalDimensions.top
        case LocalScope() => currentLocalDimensions.top
        case GroupScope() => currentGroupDimensions.top
      }
    } catch {
      case _: NoSuchElementException => throw new Exception()
    }

  def getSYCLWorkItemId(level: KernelScopeLevel, index: Int)(implicit o: Origin): Expr[Pre] =
    new Local[Pre](new DirectRef(getRangeDimensions(level)(index).variable))

  def getSYCLLinearWorkItemId(level: KernelScopeLevel)(implicit o: Origin): Expr[Pre] = try {
    val dimensions = getRangeDimensions(level)
    // See SYCL Specification section 3.11.1 for the linearization formulas
    dimensions match {
      case Seq(x) => new Local[Pre](new DirectRef(x.variable))
      case Seq(x, y) =>
        val xRef = new Local[Pre](new DirectRef(x.variable))
        val yRef = new Local[Pre](new DirectRef(y.variable))
        Plus(yRef, Mult(xRef, y.to))
      case Seq(x, y, z) =>
        val xRef = new Local[Pre](new DirectRef(x.variable))
        val yRef = new Local[Pre](new DirectRef(y.variable))
        val zRef = new Local[Pre](new DirectRef(z.variable))
        Plus(Plus(zRef, Mult(yRef, z.to)), Mult(Mult(xRef, y.to), z.to))
    }
  } catch {
    case _: NoSuchElementException => throw new Exception()
  }

  def getSYCLWorkItemRange(level: KernelScopeLevel, index: Int): Expr[Pre] =
    getRangeDimensions(level)(index).to

  def createRange(scope: KernelScopeLevel, dimension_index: Int, count: Int)(implicit o: Origin): IterVariable[Pre] = {
    if (count < 1) {
      // EW TODO: Throw custom error
      throw new Exception()
    }
    val variable = new Variable[Pre](TInt())(SourceNameOrigin(s"${scope.idName}_${dimension_index}", o))
    new IterVariable[Pre](variable, IntegerValue(0), IntegerValue(count))
  }

  def rewriteSYCLQueueSubmit(invocation: CPPInvocation[Pre]): Expr[Post] = {

    // Get the lambda describing the command group
    val commandGroupBody: Statement[Pre] = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]].body

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
    }

    val kernelDimensions = collectedKernelDeclarations.head.args.head
    val kernelDeclaration = collectedKernelDeclarations.head.args(1).asInstanceOf[CPPLambdaDefinition[Pre]]

    // Create a block of code based on what kind of kernel it is
    val kernelBody = kernelDimensions.t match {
      case primitive: CPPPrimitiveType[_] if CPP.getPrimitiveType(primitive.specifiers).isInstanceOf[SYCLTRange[_]] =>
        createBasicKernelBody(kernelDimensions, kernelDeclaration)
      case SYCLTRange(_) => createBasicKernelBody(kernelDimensions, kernelDeclaration)
//      case SYCLTNDRange(_) =>
      case _ => throw BlameUnreachable("Type mismatch", WrongKernelDimensionsType(kernelDimensions))// EW TODO implement error
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

    // Declare the newly generated kernel code inside a new method
    val proc = rw.globalDeclarations.declare(
      {
        rw.labelDecls.scope {
          new Procedure[Post](
            returnType = TRef[Post](),
            args = Seq(),
            outArgs = Nil,
            typeArgs = Nil,
            body = Some(rw.dispatch(kernelBody)),
            contract = rw.dispatch(methodContract),
          )(kernelDeclaration.blame)(InterpretedOriginVariable("VERCORS_LAMBDA_METHOD", kernelDeclaration.o))
        }
      }
    )

    // Return an invocation to the new method holding the kernel code
    ProcedureInvocation[Post](
      new DirectRef(proc), Seq(), Nil, Nil,
      invocation.givenArgs.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
      invocation.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) }
    )(invocation.blame)(invocation.o)
  }

  // EW TODO: clean up function
  def createBasicKernelBody(kernelDimensions: Expr[Pre], kernelDeclaration: CPPLambdaDefinition[Pre]): ParStatement[Pre] = {
    implicit val o: Origin = kernelDeclaration.body.o
    // Get kernel dimensions
    // EW TODO: actually check instead of assuming it is sycl range
    val range: SYCLRange[Post] = rw.dispatch(kernelDimensions).asInstanceOf[SYCLRange[Post]]

    val iters = range.dimensions.indices.map(index => createRange(GlobalScope(), index, range.dimensions(index)))
    currentGlobalDimensions.push(iters)

    val innerBody = kernelDeclaration.body.asInstanceOf[Scope[Pre]].body.asInstanceOf[Block[Pre]]
    val blockDecl = new ParBlockDecl[Pre]()(SourceNameOrigin("SYCL_BASIC_KERNEL", o))
    val UnitAccountedPredicate(contractRequires: Expr[Pre]) = kernelDeclaration.contract.requires
    val UnitAccountedPredicate(contractEnsures: Expr[Pre]) = kernelDeclaration.contract.ensures

    ParStatement[Pre](ParBlock[Pre](
      decl = blockDecl,
      iters = iters,
      // Context is already inherited
      context_everywhere = kernelDeclaration.contract.contextEverywhere,
      requires = contractRequires,
      ensures = contractEnsures,
      content = Scope[Pre](Seq(), Block[Pre](Seq(innerBody))),
    )(KernelParFailure(kernelDeclaration)))
  }

  def arrayType(t: CPPTArray[Pre]): Type[Post] = {
    // TODO: we should not use pointer here
    TPointer(rw.dispatch(t.innerType))
  }

  // EW TODO: remove?
  def lambdaType(t: CPPTLambda[Pre]): Type[Post] = TRef()

}
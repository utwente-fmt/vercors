package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin.{AbstractApplicable, Blame, CallableFailure, InterpretedOriginVariable, KernelBarrierInconsistent, KernelBarrierInvariantBroken, KernelBarrierNotEstablished, KernelPostconditionFailed, KernelPredicateNotInjective, Origin, PanicBlame, ParBarrierFailure, ParBarrierInconsistent, ParBarrierInvariantBroken, ParBarrierMayNotThrow, ParBarrierNotEstablished, ParBlockContractFailure, ParBlockFailure, ParBlockMayNotThrow, ParBlockPostconditionFailed, ParPreconditionFailed, ParPredicateNotInjective, ReceiverNotInjective, TrueSatisfiable}
import vct.col.ref.Ref
import vct.col.resolve.lang.C
import vct.col.resolve.ctx.{BuiltinField, BuiltinInstanceMethod, CNameTarget, RefADTFunction, RefAxiomaticDataType, RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCudaBlockDim, RefCudaBlockIdx, RefCudaGridDim, RefCudaThreadIdx, RefCudaVec, RefCudaVecDim, RefCudaVecX, RefCudaVecY, RefCudaVecZ, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecInvocationTarget}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.immutable.ListMap
import scala.collection.mutable

case object LangCToCol {
  case class CGlobalStateNotSupported(example: CInit[_]) extends UserError {
    override def code: String = "notSupported"
    override def text: String =
      example.o.messageInContext("Global variables in C are not supported.")
  }

  case class MultipleSharedMemoryDeclaration(decl: Node[_]) extends UserError {
    override def code: String = "multipleSharedMemoryDeclaration"
    override def text: String =
      decl.o.messageInContext(s"We don't support declaring multiple shared memory variables at a single line.")
  }

  case class WrongGPUKernelParameterType(param: CParam[_]) extends UserError {
    override def code: String = "wrongParameterType"
    override def text: String =
      param.o.messageInContext(s"This parameter has a type that is not allowed as parameter in a GPU kernel.")
  }

  case class WrongGPUType(param: CParam[_]) extends UserError {
    override def code: String = "wrongGPUType"
    override def text: String =
      param.o.messageInContext(s"This parameter has a type that is not allowed outside of a GPU kernel.")
  }

  case class WrongCType(decl: CLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongCType"
    override def text: String =
      decl.o.messageInContext(s"This declaration has a type that is not supported.")
  }

  case class WrongGPULocalType(local: CLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongGPULocalType"
    override def text: String =
      local.o.messageInContext(s"This local declaration has a type that is not allowed inside a GPU kernel.")
  }

  case class NotDynamicSharedMem(e: Expr[_]) extends UserError {
    override def code: String = "notDynamicSharedMem"
    override def text: String =
      e.o.messageInContext(s"`\\shared_mem_size` should reference a dynamic shared memory location.")
  }

  case class WrongBarrierSpecifier(b: GpgpuBarrier[_]) extends UserError {
    override def code: String = "wrongBarrierSpecifier"
    override def text: String =
      b.o.messageInContext(s"The barrier has incorrect specifiers.")
  }

  case class UnsupportedBarrierPermission(e: Node[_]) extends UserError {
    override def code: String = "unsupportedBarrierPermission"
    override def text: String =
      e.o.messageInContext(s"This is unsupported for barrier for now.")
  }

  case class RedistributingBarrier(v: CNameTarget[_], barrier: GpgpuBarrier[_], global: Boolean) extends UserError {
    def memFence: String = if(global) "CLK_GLOBAL_MEM_FENCE" else "CLK_LOCAL_MEM_FENCE"
    override def code: String = "redistributingBarrier"
    override def text: String =  barrier.o.messageInContext(
      s"Trying to redistribute the variable `$v` in a GPU barrier, but need the fence `$memFence` to do this.")
  }

  case class CDoubleContracted(decl: CGlobalDeclaration[_], defn: CFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"
    override def text: String =
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }

  case class KernelNotInjective(kernel: CGpgpuKernelSpecifier[_]) extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      kernel.blame.blame(KernelPredicateNotInjective(kernel, error.resource))
  }

  case class KernelParFailure(kernel: CGpgpuKernelSpecifier[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
      case ParPredicateNotInjective(_, predicate) =>
        kernel.blame.blame(KernelPredicateNotInjective(kernel, predicate))
      case ParPreconditionFailed(_, _) =>
        PanicBlame("Kernel parallel block precondition cannot fail, since an identical predicate is required before.").blame(error)
      case ParBlockPostconditionFailed(failure, _) =>
        kernel.blame.blame(KernelPostconditionFailed(failure, kernel))
      case ParBlockMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
    }
  }

  case class KernelBarrierFailure(barrier: GpgpuBarrier[_]) extends Blame[ParBarrierFailure] {
    override def blame(error: ParBarrierFailure): Unit = error match {
      case ParBarrierNotEstablished(failure, _) =>
        barrier.blame.blame(KernelBarrierNotEstablished(failure, barrier))
      case ParBarrierInconsistent(failure, _) =>
        barrier.blame.blame(KernelBarrierInconsistent(failure, barrier))
      case ParBarrierMayNotThrow(_) =>
        PanicBlame("Please don't throw exceptions from a gpgpu barrier, it's not polite.").blame(error)
      case ParBarrierInvariantBroken(failure, _) =>
        barrier.blame.blame(KernelBarrierInvariantBroken(failure, barrier))
    }
  }

  case class UnsupportedCast(c: CCast[_]) extends UserError {
    override def code: String = "unsupportedCast"
    override def text: String = c.o.messageInContext("This cast is not supported")
  }
}

case class LangCToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangCToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val cFunctionSuccessor: SuccessionMap[CFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()
  val cFunctionDeclSuccessor: SuccessionMap[(CGlobalDeclaration[Pre], Int), Procedure[Post]] = SuccessionMap()
  val cNameSuccessor: SuccessionMap[CNameTarget[Pre], Variable[Post]] = SuccessionMap()
  val cCurrentDefinitionParamSubstitutions: ScopedStack[Map[CParam[Pre], CParam[Pre]]] = ScopedStack()

  val cudaCurrentThreadIdx: ScopedStack[CudaVec] = ScopedStack()
  val cudaCurrentBlockIdx: ScopedStack[CudaVec] = ScopedStack()
  val cudaCurrentBlockDim: ScopedStack[CudaVec] = ScopedStack()
  val cudaCurrentGridDim: ScopedStack[CudaVec] = ScopedStack()

  val cudaCurrentGrid: ScopedStack[ParBlockDecl[Post]] = ScopedStack()
  val cudaCurrentBlock: ScopedStack[ParBlockDecl[Post]] = ScopedStack()

  private val dynamicSharedMemNames: mutable.Set[CNameTarget[Pre]] = mutable.Set()
  private val dynamicSharedMemLengthVar: mutable.Map[CNameTarget[Pre], Variable[Post]] = mutable.Map()
  private val staticSharedMemNames: mutable.Map[CNameTarget[Pre], BigInt] = mutable.Map()
  private val globalMemNames: mutable.Set[RefCParam[Pre]] = mutable.Set()
  private var kernelSpecifier: Option[CGpgpuKernelSpecifier[Pre]] = None

  case class CudaIndexVariableOrigin(dim: RefCudaVecDim[_]) extends Origin {
    override def preferredName: String =
      dim.vec.name + dim.name.toUpperCase

    override def context: String = s"At: [Variable for dimension ${dim.name} of ${dim.vec.name}]"
    override def inlineContext: String = s"[Variable for dimension ${dim.name} of ${dim.vec.name}]"
    override def shortPosition: String = "generated"
  }

  class CudaVec(ref: RefCudaVec[Pre])(implicit val o: Origin) {
    val indices: ListMap[RefCudaVecDim[Pre], Variable[Post]] =
      ListMap(Seq(RefCudaVecX[Pre](ref), RefCudaVecY[Pre](ref), RefCudaVecZ[Pre](ref)).map(
        dim => dim -> new Variable[Post](TInt())(CudaIndexVariableOrigin(dim))
      ): _*)
  }

  private def hasNoSharedMemNames(node: Node[Pre]): Boolean = {
    val allowedNonRefs = Set("get_local_id", "get_group_id", "get_local_size", "get_num_groups", "get_global_size")

    def varIsNotShared(l: CLocal[Pre]): Boolean = {
      l.ref match {
        case Some(ref: CNameTarget[Pre])
          if dynamicSharedMemNames.contains(ref) || staticSharedMemNames.contains(ref) => return false
        case None => if (!allowedNonRefs.contains(l.name)) Unreachable("Should not happen")
        case _ =>
      }
      true
    }

    node match {
      // SharedMemSize gets rewritten towards the length of a shared memory name, so is valid in global context
      case _: SharedMemSize[Pre] =>
      case l: CLocal[Pre] => return varIsNotShared(l)
      case e => if(!e.subnodes.forall(hasNoSharedMemNames)) return false
    }
    true
  }

  def rewriteUnit(cUnit: CTranslationUnit[Pre]): Unit = {
    cUnit.declarations.foreach(rw.dispatch)
  }

  def sharedSize(shared: SharedMemSize[Pre]): Expr[Post] = {
    val SharedMemSize(pointer) = shared

    val res = pointer match {
      case loc: CLocal[Pre] =>
        loc.ref flatMap { dynamicSharedMemLengthVar.get } map {v => Local[Post](v.ref)(shared.o)}
      case _ => None
    }
    res.getOrElse(throw NotDynamicSharedMem(pointer))
  }

  def cast(c: CCast[Pre]): Expr[Post] = c.t match {
    case t if t == TFloats.ieee754_64bit || t == TFloats.ieee754_32bit =>
      CastFloat[Post](rw.dispatch(c.expr), rw.dispatch(t))(c.o)
    case _ => throw UnsupportedCast(c)
  }

  def rewriteGPUParam(cParam: CParam[Pre], kernelSpecifier: CGpgpuKernelSpecifier[Pre]): Unit = {
    cParam.drop()
    val varO = InterpretedOriginVariable(C.getDeclaratorInfo(cParam.declarator).name, cParam.o)
    implicit val o: Origin = cParam.o
    val cRef = RefCParam(cParam)
    val tp = new TypeProperties(cParam.specifiers, cParam.declarator)

    kernelSpecifier match {
      case OpenCLKernel() =>
        if(tp.isGlobal && tp.arrayOrPointer && !tp.extern) globalMemNames.add(cRef)
        else if(tp.isShared && tp.arrayOrPointer && !tp.extern && tp.innerType.isDefined) {
          addDynamicShared(cRef, tp.innerType.get, varO)
          // Return, since shared memory locations are declared and initialized at thread block level, not kernel level
          return
        }
        else if(!tp.shared && !tp.global && !tp.arrayOrPointer && !tp.extern) ()
        else throw WrongGPUKernelParameterType(cParam)
      case CUDAKernel() =>
        if(!tp.global && !tp.shared && tp.arrayOrPointer && !tp.extern) globalMemNames.add(cRef)
        else if(!tp.shared && !tp.global && !tp.arrayOrPointer && !tp.extern) ()
        else throw WrongGPUKernelParameterType(cParam)
    }

    val v = new Variable[Post](cParam.specifiers.collectFirst
      { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.get)(varO)
    cNameSuccessor(cRef) = v
    rw.variables.declare(v)
  }

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    if(kernelSpecifier.isDefined) return rewriteGPUParam(cParam, kernelSpecifier.get)
    cParam.specifiers.collectFirst {
      case GPULocal() => throw WrongGPUType(cParam)
      case GPUGlobal() => throw WrongGPUType(cParam)
    }

    cParam.drop()
    val varO = InterpretedOriginVariable(C.getDeclaratorInfo(cParam.declarator).name, cParam.o)

    val v = new Variable[Post](cParam.specifiers.collectFirst
      { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.get)(varO)
    cNameSuccessor(RefCParam(cParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.get

    val (contract, subs: Map[CParam[Pre], CParam[Pre]]) = func.ref match {
      case Some(RefCGlobalDeclaration(decl, idx)) if decl.decl.contract.nonEmpty =>
        if(func.contract.nonEmpty) throw CDoubleContracted(decl, func)

        val declParams = C.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
        val defnParams = info.params.get

        (decl.decl.contract, declParams.zip(defnParams).toMap)
      case _ =>
        (func.contract, Map.empty)
    }

    val namedO = InterpretedOriginVariable(C.getDeclaratorInfo(func.declarator).name, func.o)
    val proc =
      cCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
            func.specs.collectFirst { case k: CGpgpuKernelSpecifier[Pre]
                => kernelProcedure(namedO, contract, info, Some(func.body), k) }
              .getOrElse( {
                val params = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
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
              } )
        )
      }

    cFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCGlobalDeclaration(decl, idx)) =>
        cFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def all(blame: Blame[ReceiverNotInjective])(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
    foldStar(unfoldStar(e).map(allOneExpr(blame)(idx, dim, _)))(e.o)
  }

  def allOneExpr(blame: Blame[ReceiverNotInjective])(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
    implicit val o: Origin = e.o
    val vars = findVars(e)
    val (filteredIdx, otherIdx) = idx.indices.values.zip(dim.indices.values).partition{ case (i, _) => vars.contains(i)}

    val body = otherIdx.map{case (_,range) => range}.foldLeft(e)((newE, scaleFactor) => Scale(scaleFactor.get, newE)(PanicBlame("Framed positive")) )
    if(filteredIdx.isEmpty){
      body
    } else {
      Starall(filteredIdx.map{case (v,_) => v}.toSeq, Nil, Implies(
        foldAnd(filteredIdx.map {
          case (idx, dim) => const[Post](0) <= idx.get && idx.get < dim.get
        }),
        body
      ))(blame)
    }
  }

  def findVars(e: Node[Post], vars: Set[Variable[Post]] = Set()): Set[Variable[Post]] = e match {
      case Local(ref) => vars + ref.decl
      case _ => e.transSubnodes.collect { case Local(Ref(v)) => v}.toSet
  }

  def allThreadsInBlock(blame: Blame[ReceiverNotInjective])(e: Expr[Pre]): Expr[Post] = {
    val thread = new CudaVec(RefCudaThreadIdx())(e.o)
    cudaCurrentThreadIdx.having(thread) { all(blame)(thread, cudaCurrentBlockDim.top, rw.dispatch(e)) }
  }

  def allThreadsInGrid(blame: Blame[ReceiverNotInjective])(e: Expr[Pre]): Expr[Post] = {
    val block = new CudaVec(RefCudaBlockIdx())(e.o)
    cudaCurrentBlockIdx.having(block) { all(blame)(block, cudaCurrentGridDim.top, allThreadsInBlock(blame)(e)) }
  }

  def getCDecl(d: CNameTarget[Pre]): CDeclarator[Pre] = d match {
    case RefCParam(decl) => decl.declarator
    case RefCFunctionDefinition(decl) => decl.declarator
    case RefCGlobalDeclaration(decls, initIdx) => decls.decl.inits(initIdx).decl
    case RefCLocalDeclaration(decls, initIdx) => decls.decl.inits(initIdx).decl
    case _ => throw Unreachable("Should not happen")
  }

  def getInnerType(t: Type[Post]): Type[Post] = t match {
    case TArray(element) => element
    case TPointer(element) => element
    case _ => throw Unreachable("Already checked on pointer or array type")
  }

  def declareSharedMemory(): (Seq[Variable[Post]], Seq[Statement[Post]]) = rw.variables.collect {
    var result: Seq[Statement[Post]] = Seq()
    dynamicSharedMemNames.foreach(d =>
    {
      implicit val o: Origin = getCDecl(d).o
      val varO: Origin = InterpretedOriginVariable(s"${C.getDeclaratorInfo(getCDecl(d)).name}_size", o)
      val v = new Variable[Post](TInt())(varO)
      dynamicSharedMemLengthVar(d) = v
      rw.variables.declare(v)
      val decl: Statement[Post] = LocalDecl(cNameSuccessor(d))
      val assign: Statement[Post] = assignLocal(Local(cNameSuccessor(d).ref),
        NewArray[Post](getInnerType(cNameSuccessor(d).t), Seq(Local(v.ref)), 0))
      result ++= Seq(decl, assign)
    })
    staticSharedMemNames.foreach{case (d,size) =>
    implicit val o: Origin = getCDecl(d).o
      val decl: Statement[Post] = LocalDecl(cNameSuccessor(d))
      val assign: Statement[Post] = assignLocal(Local(cNameSuccessor(d).ref),
        NewArray[Post](getInnerType(cNameSuccessor(d).t), Seq(IntegerValue(size)), 0))
      result ++= Seq(decl, assign)
    }

    result
  }

  def kernelProcedure(o: Origin, contract: ApplicableContract[Pre], info: C.DeclaratorInfo[Pre], body: Option[Statement[Pre]]
                     , kernelSpec: CGpgpuKernelSpecifier[Pre]): Procedure[Post] = {
    dynamicSharedMemNames.clear()
    staticSharedMemNames.clear()
    kernelSpecifier = Some(kernelSpec)

    val blockDim = new CudaVec(RefCudaBlockDim())(o)
    val gridDim = new CudaVec(RefCudaGridDim())(o)
    cudaCurrentBlockDim.having(blockDim) {
      cudaCurrentGridDim.having(gridDim) {
        val args = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
        rw.variables.collect { dynamicSharedMemNames.foreach(d => rw.variables.declare(cNameSuccessor(d)) ) }
        rw.variables.collect { staticSharedMemNames.foreach(d => rw.variables.declare(cNameSuccessor(d._1)) ) }
        val implFiltered = body.map(init => filterSharedDecl(init))
        val (sharedMemSizes, sharedMemInit: Seq[Statement[Post]]) = declareSharedMemory()

        val newArgs = blockDim.indices.values.toSeq ++ gridDim.indices.values.toSeq ++ args ++ sharedMemSizes
        val newGivenArgs = rw.variables.dispatch(contract.givenArgs)
        val newYieldsArgs = rw.variables.dispatch(contract.yieldsArgs)
        // We add the requirement that a GPU kernel must always have threads (non zero block or grid dimensions)
        val nonZeroThreads: Expr[Post] = foldStar(
            (blockDim.indices.values ++ gridDim.indices.values)
              .map( v => Less(IntegerValue(0)(o), v.get(o))(o))
              .toSeq)(o)
        val UnitAccountedPredicate(contractRequires: Expr[Pre]) = contract.requires
        val UnitAccountedPredicate(contractEnsures: Expr[Pre]) = contract.ensures

        val parBody = body.map(impl => {
          implicit val o: Origin = impl.o
          val threadIdx = new CudaVec(RefCudaThreadIdx())
          val blockIdx = new CudaVec(RefCudaBlockIdx())
          val gridDecl = new ParBlockDecl[Post]()
          val blockDecl = new ParBlockDecl[Post]()

          cudaCurrentThreadIdx.having(threadIdx) {
            cudaCurrentBlockIdx.having(blockIdx) {
              cudaCurrentGrid.having(gridDecl) {
                cudaCurrentBlock.having(blockDecl) {
                  val contextBlock = foldStar(unfoldStar(contract.contextEverywhere)
                    .filter(hasNoSharedMemNames).map(allThreadsInBlock(KernelNotInjective(kernelSpec))))
                  val innerContent = ParStatement(ParBlock(
                      decl = blockDecl,
                      iters = threadIdx.indices.values.zip(blockDim.indices.values).map {
                        case (index, dim) => IterVariable(index, const(0), dim.get)
                      }.toSeq,
                      // Context is already inherited
                      context_everywhere = Star(nonZeroThreads, rw.dispatch(contract.contextEverywhere)),
                      requires = rw.dispatch(contractRequires),
                      ensures = rw.dispatch(contractEnsures),
                      content = rw.dispatch(implFiltered.get),
                    )(KernelParFailure(kernelSpec)))
                  ParStatement(ParBlock(
                    decl = gridDecl,
                    iters = blockIdx.indices.values.zip(gridDim.indices.values).map {
                      case (index, dim) => IterVariable(index, const(0), dim.get)
                    }.toSeq,
                    // Context is added to requires and ensures here
                    context_everywhere = tt,
                    requires = Star(nonZeroThreads,
                        Star(contextBlock,
                          foldStar(
                            unfoldStar(contractRequires)
                              .filter(hasNoSharedMemNames)
                              .map(allThreadsInBlock(KernelNotInjective(kernelSpec)))))
                        ),
                    ensures = Star(nonZeroThreads,
                      Star(contextBlock,
                        foldStar(
                          unfoldStar(contractEnsures)
                            .filter(hasNoSharedMemNames)
                            .map(allThreadsInBlock(KernelNotInjective(kernelSpec)))) )
                      ),
                    // Add shared memory initialization before beginning of inner parallel block
                    content = Block[Post](sharedMemInit ++ Seq(innerContent))
                  )(KernelParFailure(kernelSpec)))
                }
              }
            }
          }
        })

        val gridContext: Expr[Post] =
          foldStar(unfoldStar(contract.contextEverywhere)
            .filter(hasNoSharedMemNames)
            .map(allThreadsInGrid(KernelNotInjective(kernelSpec))))(o)
        val requires: Expr[Post] = foldStar(Seq(gridContext, nonZeroThreads)
          ++ unfoldStar(contractRequires).filter(hasNoSharedMemNames).map(allThreadsInGrid(KernelNotInjective(kernelSpec))) )(o)
        val ensures: Expr[Post] = foldStar(Seq(gridContext, nonZeroThreads)
          ++ unfoldStar(contractEnsures).filter(hasNoSharedMemNames).map(allThreadsInGrid(KernelNotInjective(kernelSpec))) )(o)
        val result = new Procedure[Post](
          returnType = TVoid(),
          args = newArgs,
          outArgs = Nil, typeArgs = Nil,
          body = parBody,
          contract = ApplicableContract(
            UnitAccountedPredicate(requires)(o),
            UnitAccountedPredicate(ensures)(o),
            // Context everywhere is already passed down in the body
            tt,
            contract.signals.map(rw.dispatch),
            newGivenArgs,
            newYieldsArgs,
            contract.decreases.map(rw.dispatch),
          )(contract.blame)(contract.o)
        )(AbstractApplicable)(o)
        kernelSpecifier = None

        result
      }
    }
  }

  class TypeProperties(specs: Seq[CDeclarationSpecifier[Pre]], decl: CDeclarator[Pre]){
    var arrayOrPointer = false
    var global = false
    var shared = false
    var extern = false
    var innerType: Option[Type[Pre]] = None
    var arraySize: Option[Expr[Pre]] = None

    specs.foreach {
      case GPULocal() => shared = true
      case GPUGlobal() => global = true
      case CSpecificationType(CTPointer(t)) =>
        arrayOrPointer = true
        innerType = Some(t)
      case CSpecificationType(CTArray(size, t)) =>
        arraySize = size
        innerType = Some(t)
        arrayOrPointer = true
      case CExtern() => extern = true
      case _ =>
    }

    def isShared: Boolean = shared && !global
    def isGlobal: Boolean = !shared && global
  }

  def addDynamicShared(cRef: CNameTarget[Pre], t: Type[Pre], o: Origin): Unit = {
    dynamicSharedMemNames.add(cRef)
    val v = new Variable[Post](TArray[Post](rw.dispatch(t)))(o)
    cNameSuccessor(cRef) = v
  }

  def addStaticShared(decl:  CDeclarator[Pre], cRef: CNameTarget[Pre], t: Type[Pre], o: Origin,
                      declStatement: CLocalDeclaration[Pre], arraySize: Option[Expr[Pre]]): Unit = arraySize match {
      case Some(IntegerValue(size)) =>
        val v = new Variable[Post](TArray[Post](rw.dispatch(t)))(o)
        staticSharedMemNames(cRef) = size
        cNameSuccessor(cRef) = v
      case _ => throw WrongGPULocalType(declStatement)
  }

  def isShared(s: Statement[Pre]): Boolean = {
    if(!s.isInstanceOf[CDeclarationStatement[Pre]])
      return false

    val CDeclarationStatement(decl) = s

    if (decl.decl.inits.size != 1)
      throw MultipleSharedMemoryDeclaration(decl)

    val prop = new TypeProperties(decl.decl.specs, decl.decl.inits.head.decl)
    if (!prop.shared) return false
    val init: CInit[Pre] = decl.decl.inits.head
    val varO = InterpretedOriginVariable(C.getDeclaratorInfo(init.decl).name, decl.o)
    val cRef = RefCLocalDeclaration(decl, 0)

    kernelSpecifier match {
      case Some(CUDAKernel()) =>
        if (!prop.global && prop.arrayOrPointer && prop.innerType.isDefined && prop.extern) {
          addDynamicShared(cRef, prop.innerType.get, varO)
          return true
        }
        if (!prop.global && prop.arrayOrPointer && prop.innerType.isDefined && !prop.extern) {
          addStaticShared(init.decl, cRef, prop.innerType.get, varO, decl, prop.arraySize)
          return true
        }
      case Some(OpenCLKernel()) =>
        if (!prop.global && prop.arrayOrPointer && prop.innerType.isDefined && !prop.extern) {
          addStaticShared(init.decl, cRef, prop.innerType.get, varO, decl, prop.arraySize)
          return true
        }
      case None => throw Unreachable(f"This should have been called from inside a GPU kernel scope.")
    }

    // We are shared, but couldn't add it
    throw WrongGPULocalType(decl)
  }

  def filterSharedDecl(s: Statement[Pre]): Statement[Pre] = {
    s match {
      case Scope(locals, block@Block(stats)) =>
        Scope(locals, Block(stats.filterNot(isShared))(block.o))(s.o)
      case _ => s
    }
  }

  def rewriteGlobalDecl(decl: CGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.get
    for((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty , skip the declaration: the definition is used instead.
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) =>
          cFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            decl.decl.specs.collectFirst { case k: CGpgpuKernelSpecifier[Pre]
                => kernelProcedure(init.o, decl.decl.contract, info, None, k) }
              .getOrElse(
                new Procedure[Post](
                  returnType = t,
                  args = rw.variables.collect { params.foreach(rw.dispatch) }._1,
                  outArgs = Nil,
                  typeArgs = Nil,
                  body = None,
                  contract = rw.dispatch(decl.decl.contract),
                )(AbstractApplicable)(init.o)
              )
          )
        case None =>
          throw CGlobalStateNotSupported(init)
      }
    }
  }

  def rewriteLocal(decl: CLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.get
    decl.decl.specs.foreach {
      case _: CSpecificationType[Pre] =>
      case _ => throw WrongCType(decl)
    }

    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head

    val info = C.getDeclaratorInfo(init.decl)
    val varO: Origin = InterpretedOriginVariable(info.name, init.o)
    t match {
      case CTArray(Some(size), t) =>
        if(init.init.isDefined) throw WrongCType(decl)
        implicit val o: Origin = init.o
        val v = new Variable[Post](TArray(t))(varO)
        cNameSuccessor(RefCLocalDeclaration(decl, 0)) = v
        val newArr = NewArray[Post](t, Seq((size)), 0)
        Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
      case _ =>
        val v = new Variable[Post](t)(varO)
        cNameSuccessor(RefCLocalDeclaration(decl, 0)) = v
        implicit val o: Origin = init.o
        init.init
          .map(value => Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value)))))
          .getOrElse(LocalDecl(v))
    }
  }

  def rewriteGoto(goto: CGoto[Pre]): Statement[Post] =
    Goto[Post](rw.succ(goto.ref.getOrElse(???)))(goto.o)

  def gpuBarrier(barrier: GpgpuBarrier[Pre]): Statement[Post] = {
    implicit val o: Origin = barrier.o

    var globalFence = false
    var localFence = false

    barrier.specifiers.foreach {
      case GpuLocalMemoryFence() => localFence = true
      case GpuGlobalMemoryFence() => globalFence = true
      case GpuZeroMemoryFence(i) => if(i != 0) throw WrongBarrierSpecifier(barrier)
    }
    if(!globalFence || !localFence){
      val redist = permissionScanner(barrier)
      if(!globalFence)
        redist
        .intersect(globalMemNames.toSet)
        .foreach(v => throw RedistributingBarrier(v, barrier, global = true))
      if(!localFence)
        redist
          .intersect(dynamicSharedMemNames.union(dynamicSharedMemNames).toSet)
          .foreach(v => throw RedistributingBarrier(v, barrier, global = false))
    }

    ParBarrier[Post](
      block = cudaCurrentBlock.top.ref,
      invs = Nil,
      requires = rw.dispatch(barrier.requires),
      ensures = rw.dispatch(barrier.ensures),
      content = Block(Nil),
    )(KernelBarrierFailure(barrier))
  }

  def isPointer(t: Type[Pre]) : Boolean = t match {
    case TPointer(_) => true
    case CPrimitiveType(specs) =>
      specs.collectFirst{case CSpecificationType(TPointer(_)) => }.nonEmpty
    case _ => false
  }

  def isNumeric(t: Type[Pre]): Boolean = t match{
    case _: NumericType[Pre] => true
    case CPrimitiveType(specs) =>
      specs.collectFirst{case CSpecificationType(_ : NumericType[Pre]) =>}.nonEmpty
    case _ => false
  }

  def searchNames(e: Expr[Pre], original: Node[Pre]): Seq[CNameTarget[Pre]] = e match {
    case arr : CLocal[Pre] => Seq(arr.ref.get)
    case PointerAdd(arr : CLocal[Pre], _) => Seq(arr.ref.get)
    case AmbiguousSubscript(arr : CLocal[Pre], _) => Seq(arr.ref.get)
    case AmbiguousPlus(l, r) if isPointer(l.t) && isNumeric(r.t) => searchNames(l, original)
    case _ => throw UnsupportedBarrierPermission(original)
  }

  def searchNames(loc: Location[Pre], original: Node[Pre]): Seq[CNameTarget[Pre]] = loc match {
    case ArrayLocation(arr : CLocal[Pre], _) => Seq(arr.ref.get)
    case PointerLocation(arr : CLocal[Pre]) => Seq(arr.ref.get)
    case PointerLocation(PointerAdd(arr : CLocal[Pre], _)) => Seq(arr.ref.get)
    case AmbiguousLocation(expr) => searchNames(expr, original)
    case _ => throw UnsupportedBarrierPermission(original)
  }

  def searchPermission(e: Node[Pre]): Seq[CNameTarget[Pre]] = {
    e match {
      case e: Expr[Pre] if e.t != TResource[Pre]() => Seq()
      case Perm(loc, _) => searchNames(loc, e)
      case PointsTo(loc, _, _) => searchNames(loc, e)
      case CurPerm(loc) => searchNames(loc, e)
      case PermPointer(pointer, _, _) => searchNames(pointer, e)
      case PermPointerIndex(pointer, _, _) => searchNames(pointer, e)
      case _ => e.subnodes.flatMap(searchPermission)
    }
  }

  def permissionScanner(barrier: GpgpuBarrier[Pre]): Set[CNameTarget[Pre]] ={
    val pres = unfoldStar(barrier.requires).toSet
    val posts = unfoldStar(barrier.ensures).toSet
    val context = pres intersect posts
    // Only scan the non context permissions
    val nonContext = (pres union posts) diff context
    nonContext.flatMap(searchPermission)
  }

  def result(ref: RefCFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cFunctionSuccessor.ref(ref.decl))

  def result(ref: RefCGlobalDeclaration[Pre])(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cFunctionSuccessor.ref(defn.decl))
      case None => Result[Post](cFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def local(local: CLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case RefAxiomaticDataType(_) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCParam[Pre] =>
        if(cCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cNameSuccessor.ref(RefCParam(cCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cNameSuccessor.ref(ref))
      case RefCFunctionDefinition(_) => throw NotAValue(local)
      case RefCGlobalDeclaration(_, _) => throw NotAValue(local)
      case ref: RefCLocalDeclaration[Pre] => Local(cNameSuccessor.ref(ref))
      case _: RefCudaVec[Pre] => throw NotAValue(local)
    }
  }

  def deref(deref: CStructAccess[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o

    def getCuda(dim: RefCudaVecDim[Pre]): Expr[Post] = dim.vec match {
      case RefCudaThreadIdx() => cudaCurrentThreadIdx.top.indices(dim).get
      case RefCudaBlockIdx() => cudaCurrentBlockIdx.top.indices(dim).get
      case RefCudaBlockDim() => cudaCurrentBlockDim.top.indices(dim).get
      case RefCudaGridDim() => cudaCurrentGridDim.top.indices(dim).get
    }

    deref.ref.get match {
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(deref.blame)
      case BuiltinField(f) => rw.dispatch(f(deref.struct))
      case target: SpecInvocationTarget[Pre] => ???
      case dim: RefCudaVecDim[Pre] => getCuda(dim)
    }
  }

  def invocation(inv: CInvocation[Pre]): Expr[Post] = {
    val CInvocation(applicable, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefFunction(decl) =>
        FunctionInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefProcedure(decl) =>
        ProcedureInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
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
      case ref: RefCFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](cFunctionSuccessor.ref(ref.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case e: RefCGlobalDeclaration[Pre] => globalInvocation(e, inv)

    }
  }

  /**
    * Rewrites a CudaKernelInvocation to a procedure.
    * @param kernel - the invocation we want to rewrite
    * @return a Procedure[Post]
    */
  def cudaKernelInvocation(kernel: GpgpuCudaKernelInvocation[Pre]): Expr[Post] = {
    val GpgpuCudaKernelInvocation(ker, blocks, threads, args, givenMap, yields) = kernel
    implicit val o: Origin = kernel.o
    val one = const[Post](1)
    kernel.ref.get match {
      case target: SpecInvocationTarget[_] => ???
      case ref: RefCFunctionDefinition[Pre] =>
        ProcedureInvocation[Post](cFunctionSuccessor.ref(ref.decl),
          rw.dispatch(blocks) +: one +:  one
            +: rw.dispatch(threads) +: one +: one +: args.map(rw.dispatch),
          Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(kernel.blame)
      case e: RefCGlobalDeclaration[Pre] => ???
    }
  }

  /**
    * Returns the local id of a thread in a given dimension.
    * @param index - the dimension for which we want the thread id
    * @param origin - the origin of the expr node
    * @return the value of the thread ID in the given dimension
    */
  def getCudaLocalThread(index: Int, origin: Origin): Expr[Post]  = {
    implicit val o: Origin = origin
    cudaCurrentThreadIdx.top.indices.values.toSeq.apply(index).get
  }

  /**
    * Returns the group id of a thread in a given dimension.
    * @param index - the dimension for which we want the thread id
    * @param origin - the origin of the expr node
    * @return the value of the thread ID in the given dimension
    */
  def getCudaGroupThread(index: Int, origin: Origin): Expr[Post]  = {
    implicit val o: Origin = origin
    cudaCurrentBlockIdx.top.indices.values.toSeq.apply(index).get
  }

  /**
    * Returns the local size of a given dimension.
    * @param index - the dimension for which we query its size
    * @param origin - the origin of the expr node
    * @return the value of the size of the given dimension.
    */
  def getCudaLocalSize(index: Int, origin: Origin): Expr[Post] = {
    implicit val o: Origin = origin
    cudaCurrentBlockDim.top.indices.values.toSeq.apply(index).get
  }

  /**
    * Returns the global size of a given dimension.
    * @param index - the dimension for which we query the global size.
    * @param o - the origin of the expr node
    * @return the value of the global size of the given dimension.
    */
  def getCudaGroupSize(index: Int, o: Origin): Expr[Post] = {
    implicit val origin: Origin = o
    cudaCurrentGridDim.top.indices.values.toSeq.apply(index).get
  }

  /**
    * Rewrites a LocalThreadId and translates it to a linear ID value. The expression is equivalent to the openCL
    *   method "get_local_linear_id".
    * @param local - the node we want to rewrite
    * @return an expression equal to the linear ID of the local thread
    */
  def cudaLocalThreadId(local: LocalThreadId[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Plus(
      Mult(
        Mult(getCudaLocalThread(2, o), getCudaLocalSize(1, o)), // get_local_id(2) * get_local_size(1)
      getCudaLocalSize(0, o)                                           //  * get_local_size(0)
      ),                                                                      //  +
      Plus(
        Mult(getCudaLocalThread(1, o), getCudaLocalSize(0, o)), //  get_local_id(1) * get_local_size(0)
        getCudaLocalThread(0, o)                                       //  + get_local_id(0)
      )
    )
  }

  /**
    * Rewrites a GlobalThreadId into a linear ID value. The expression is equivalent to the openCL
    *   method "get_global_linear_id".
    * We assume the global_offset is 0 in our calculation.
    * @param global - the node we want to rewrite
    * @return an expression equal to the linear global id of the thread.
    */
  def cudaGlobalThreadId(global: GlobalThreadId[Pre]): Expr[Post] = {
    implicit val o: Origin = global.o
    Plus(
      Plus(
        Mult(
          Mult(getCudaGroupThread(2, o), getCudaGroupSize(1, o)),   // (get_global_id(2) * get_global_size(1)
          getCudaGroupSize(0, o)),                                         //   * get_global_size(0)
        Mult(getCudaGroupThread(1, o), getCudaGroupSize(0, o))),    //   + get_global_id(1) * get_global_size(0)
      getCudaGroupThread(0, o)                                             //   + get_global_id(0)
    )
  }

  def globalInvocation(e: RefCGlobalDeclaration[Pre], inv: CInvocation[Pre]): Expr[Post] = {
    val CInvocation(_, args, givenMap, yields) = inv
    val RefCGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    val arg = if(args.size == 1){
      args.head match {
        case IntegerValue(i) if i >= 0 && i < 3 => Some(i.toInt)
        case _ => None
      }
    } else None
    (e.name, arg) match {
      case ("get_local_id", Some(i)) => getCudaLocalThread(i, o)
      case ("get_group_id", Some(i)) => getCudaGroupThread(i, o)
      case ("get_local_size", Some(i)) => getCudaLocalSize(i, o)
      case ("get_global_size", Some(i)) => getCudaGroupSize(i, o)
      case ("get_num_groups", Some(i)) => getCudaGroupSize(i, o)
      case _ => ProcedureInvocation[Post](cFunctionDeclSuccessor.ref((decls, initIdx)), args.map(rw.dispatch), Nil, Nil,
        givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
        yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
    }
  }

  def pointerType(t: CTPointer[Pre]): Type[Post] = {
    TPointer(rw.dispatch(t.innerType))
  }

  def arrayType(t: CTArray[Pre]): Type[Post] = {
    // TODO: we should not use pointer here
    TPointer(rw.dispatch(t.innerType))
  }
}

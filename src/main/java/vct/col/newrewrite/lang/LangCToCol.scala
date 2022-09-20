package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin.{AbstractApplicable, InterpretedOriginVariable, Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, C, CNameTarget, RefADTFunction, RefAxiomaticDataType,
  RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCudaBlockDim, RefCudaBlockIdx,
  RefCudaGridDim, RefCudaThreadIdx, RefCudaVec, RefCudaVecDim, RefCudaVecX, RefCudaVecY, RefCudaVecZ, RefFunction,
  RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess,
  RefPredicate, RefProcedure, RefVariable, SpecInvocationTarget}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.parsers.Language
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.immutable.ListMap
import scala.collection.mutable

case object LangCToCol {
  case class CGlobalStateNotSupported(example: CInit[_]) extends UserError {
    override def code: String = "notSupported"
    override def text: String =
      example.o.messageInContext("Global variables in C are not supported.")
  }

  case class WrongGPUKernelParameterType(param: CParam[_]) extends UserError {
    override def code: String = "wrongParameterType"
    override def text: String = s"The parameter `$param` has a type that is not allowed`as parameter in a GPU kernel."
  }

  case class WrongGPUType(param: CParam[_]) extends UserError {
    override def code: String = "wrongGPUType"
    override def text: String = s"The parameter `$param` has a type that is not allowed`outside of a GPU kernel."
  }

  case class NotDynamicSharedMem(e: Expr[_]) extends UserError {
    override def code: String = "notDynamicSharedMem"
    override def text: String = s"The expression \\shared_mem_size(`$e`) is not referencing to a shared memory location."
  }

  case class WrongBarrierSpecifier(b: GpgpuBarrier[_]) extends UserError {
    override def code: String = "wrongBarrierSpecifier"
    override def text: String = s"The barrier `$b` has incorrect specifiers."
  }

  case class UnsupportedBarrierPermission(e: Node[_]) extends UserError {
    override def code: String = "unsupportedBarrierPermission"
    override def text: String = s"The permission `$e` is unsupported for barrier for now."
  }

  case class RedistributingBarrier(v: CNameTarget[_], global: Boolean) extends UserError {
    def memFence: String = if(global) "CLK_GLOBAL_MEM_FENCE" else "CLK_LOCAL_MEM_FENCE"
    override def code: String = "redistributingBarrier"
    override def text: String = s"Trying to redistribute the variable `$v` in a GPU barrier, but need the fence `$memFence` to do this."
  }

  case class CDoubleContracted(decl: CGlobalDeclaration[_], defn: CFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"
    override def text: String =
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }
}

case class LangCToCol[Pre <: Generation](rw: LangSpecificToCol[Pre], language: Option[Language]) extends LazyLogging {
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

  private val dynamicSharedMemNames: mutable.Set[RefCParam[Pre]] = mutable.Set()
  private val dynamicSharedMemLengthVar: mutable.Map[CNameTarget[Pre], Variable[Post]] = mutable.Map()
  private val staticSharedMemNames: mutable.Set[RefCParam[Pre]] = mutable.Set()
  private val globalMemNames: mutable.Set[RefCParam[Pre]] = mutable.Set()
  private var inKernel: Boolean = false

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
    val allowedNonRefs = Set("get_local_id", "get_group_id", "get_local_size", "get_num_groups")

    def varIsNotShared(l: CLocal[Pre]): Boolean = {
      l.ref match {
        case Some(ref: RefCParam[Pre])
          if dynamicSharedMemNames.contains(ref) || staticSharedMemNames.contains(ref) => return false
        case None => if (!allowedNonRefs.contains(l.name)) ???
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

  def rewriteGPUParam(cParam: CParam[Pre]): Unit = {
    cParam.drop()
    val varO = InterpretedOriginVariable(C.getDeclaratorInfo(cParam.declarator).name, cParam.o)
    implicit val o: Origin = cParam.o

    var arrayOrPointer = false
    var global = false
    var shared = false
    var extern = false
    var innerType: Option[Type[Pre]] = None

    val cRef = RefCParam(cParam)

    cParam.specifiers.foreach{
      case GPULocal() => shared = true
      case GPUGlobal() => global = true
      case CSpecificationType(TPointer(t)) =>
        arrayOrPointer = true
        innerType = Some(t)
      case CSpecificationType(TArray(t)) =>
        arrayOrPointer = true
        innerType = Some(t)
      case CExtern() => extern = true
      case _ =>
    }

    language match {
      case Some(Language.C) =>
        if(global && !shared && arrayOrPointer && !extern) globalMemNames.add(cRef)
        else if(shared && !global && arrayOrPointer && !extern) {
          dynamicSharedMemNames.add(cRef)
          // Create Var with array type here and return
          val v = new Variable[Post](TArray[Post](rw.dispatch(innerType.get)) )(varO)
          cNameSuccessor(cRef) = v
          return
        }
        else if(!shared && !global && !arrayOrPointer && !extern) ()
        else throw WrongGPUKernelParameterType(cParam)
      case Some(Language.CUDA) =>
        if(!global && !shared && arrayOrPointer && !extern) globalMemNames.add(cRef)
        else if(!shared && !global && !arrayOrPointer && !extern) ()
        else throw WrongGPUKernelParameterType(cParam)
      case _ => throw Unreachable(f"The language '$language' should not have GPU kernels.'")
    }

    val v = new Variable[Post](cParam.specifiers.collectFirst
      { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???))(varO)
    cNameSuccessor(cRef) = v
    rw.variables.declare(v)
  }

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    if(inKernel) return rewriteGPUParam(cParam)
    cParam.specifiers.collectFirst{
      case GPULocal() => throw WrongGPUType(cParam)
      case GPUGlobal() => throw WrongGPUType(cParam)
    }

    cParam.drop()
    val varO = InterpretedOriginVariable(C.getDeclaratorInfo(cParam.declarator).name, cParam.o)

    val v = new Variable[Post](cParam.specifiers.collectFirst
      { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???))(varO)
    cNameSuccessor(RefCParam(cParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)

    val (contract, subs: Map[CParam[Pre], CParam[Pre]]) = func.ref match {
      case Some(RefCGlobalDeclaration(decl, idx)) if decl.decl.contract.nonEmpty =>
        if(func.contract.nonEmpty) throw CDoubleContracted(decl, func)

        val declParams = C.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
        val defnParams = info.params.get

        (decl.decl.contract, declParams.zip(defnParams).toMap)
      case _ =>
        (func.contract, Map.empty)
    }

    val proc =
      cCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
          if (func.specs.collectFirst { case CKernel() => () }.nonEmpty) {
            val namedO = InterpretedOriginVariable(C.getDeclaratorInfo(func.declarator).name, func.o)
            kernelProcedure(namedO, contract, info, Some(func.body))
          } else {
            val params = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
            new Procedure[Post](
              returnType = returnType,
              args = params,
              outArgs = Nil,
              typeArgs = Nil,
              body = Some(rw.dispatch(func.body)),
              contract = rw.dispatch(contract),
            )(func.blame)(func.o)
          }
        )
      }

    cFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCGlobalDeclaration(decl, idx)) =>
        cFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def all(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
    foldStar(unfoldStar(e).map(allOneExpr(idx, dim, _)))(e.o)
  }

  def allOneExpr(idx: CudaVec, dim: CudaVec, e: Expr[Post]): Expr[Post] = {
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
      ))(PanicBlame("Where blame"))
    }
  }

  def findVars(e: Node[Post], vars: Set[Variable[Post]] = Set()): Set[Variable[Post]] = e match {
      case Local(ref) => vars + ref.decl
      case _ => e.transSubnodes.collect { case Local(Ref(v)) => v}.toSet
  }

  def allThreadsInBlock(e: Expr[Pre]): Expr[Post] = {
    val thread = new CudaVec(RefCudaThreadIdx())(e.o)
    cudaCurrentThreadIdx.having(thread) { all(thread, cudaCurrentBlockDim.top, rw.dispatch(e)) }
  }

  def allThreadsInGrid(e: Expr[Pre]): Expr[Post] = {
    val block = new CudaVec(RefCudaBlockIdx())(e.o)
    cudaCurrentBlockIdx.having(block) { all(block, cudaCurrentGridDim.top, allThreadsInBlock(e)) }
  }

  def kernelProcedure(o: Origin, contract: ApplicableContract[Pre], info: C.DeclaratorInfo[Pre], body: Option[Statement[Pre]]): Procedure[Post] = {
    dynamicSharedMemNames.clear()
    staticSharedMemNames.clear()
    inKernel = true

    val blockDim = new CudaVec(RefCudaBlockDim())(o)
    val gridDim = new CudaVec(RefCudaGridDim())(o)
    cudaCurrentBlockDim.having(blockDim) {
      cudaCurrentGridDim.having(gridDim) {
        val args = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
        rw.variables.collect { dynamicSharedMemNames.foreach(d => rw.variables.declare(cNameSuccessor(d)) ) }
        val (sharedMemSizes, sharedMemInit: Seq[Statement[Post]]) = rw.variables.collect {
          var result: Seq[Statement[Post]] = Seq()
          dynamicSharedMemNames.foreach(d =>
          {
            implicit val o: Origin = d.decl.o
            val varO: Origin = InterpretedOriginVariable(s"${C.getDeclaratorInfo(d.decl.declarator).name}_size", d.decl.o)
            val v = new Variable[Post](TInt())(varO)
            dynamicSharedMemLengthVar(d) = v
            rw.variables.declare(v)
            val decl: Statement[Post] = LocalDecl(cNameSuccessor(d))
            val assign: Statement[Post] = Assign[Post](Local(cNameSuccessor(d).ref)
              , NewArray[Post](v.t, Seq(Local(v.ref)), 0))(PanicBlame("Assign should work"))
            result ++= Seq(decl, assign)
          })
          result
        }

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
                    .filter(hasNoSharedMemNames).map(allThreadsInBlock))
                  val innerContent = ParStatement(ParBlock(
                      decl = blockDecl,
                      iters = threadIdx.indices.values.zip(blockDim.indices.values).map {
                        case (index, dim) => IterVariable(index, const(0), dim.get)
                      }.toSeq,
                      // Context is already inherited
                      context_everywhere = Star(nonZeroThreads, rw.dispatch(contract.contextEverywhere)),
                      requires = rw.dispatch(contractRequires),
                      ensures = rw.dispatch(contractEnsures),
                      content = rw.dispatch(impl),
                    )(PanicBlame("where blame?")))
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
                              .map(allThreadsInBlock)))
                        ),
                    ensures = Star(nonZeroThreads,
                      Star(contextBlock,
                        foldStar(
                          unfoldStar(contractEnsures)
                            .filter(hasNoSharedMemNames)
                            .map(allThreadsInBlock)) )
                      ),
                    // Add shared memory initialization before beginning of inner parallel block
                    content = Block[Post](sharedMemInit ++ Seq(innerContent))
                  )(PanicBlame("where blame?")))
                }
              }
            }
          }
        })

        val gridContext: Expr[Post] =
          foldStar(unfoldStar(contract.contextEverywhere)
            .filter(hasNoSharedMemNames)
            // TODO: unsure if we need to map over all threads. context anywhere feels like it should apply as general knowledge unrelated to thread ids
            .map(allThreadsInGrid))(o)
        val requires: Expr[Post] = foldStar(Seq(gridContext, nonZeroThreads)
          ++ unfoldStar(contractRequires).filter(hasNoSharedMemNames).map(allThreadsInGrid) )(o)
        val ensures: Expr[Post] = foldStar(Seq(gridContext, nonZeroThreads)
          ++ unfoldStar(contractEnsures).filter(hasNoSharedMemNames).map(allThreadsInGrid) )(o)
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
        inKernel = false

        result
      }
    }
  }

  def rewriteGlobalDecl(decl: CGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    for((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty , skip the declaration: the definition is used instead.
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) =>
          cFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            if(decl.decl.specs.collectFirst { case CKernel() => () }.nonEmpty) {
              kernelProcedure(init.o, decl.decl.contract, info, None)
            } else {
              new Procedure[Post](
                returnType = t,
                args = rw.variables.collect { params.foreach(rw.dispatch) }._1,
                outArgs = Nil,
                typeArgs = Nil,
                body = None,
                contract = rw.dispatch(decl.decl.contract),
              )(AbstractApplicable)(init.o)
            }
          )
        case None =>
          throw CGlobalStateNotSupported(init)
      }
    }
  }

  def rewriteLocal(decl: CLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    // PB: this is correct because Seq[CInit]'s are flattened, but the structure is a bit stupid.
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    Block(for((init, idx) <- decl.decl.inits.zipWithIndex) yield {
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) => ???
        case None =>
          val varO: Origin = InterpretedOriginVariable(C.getDeclaratorInfo(init.decl).name, init.o)
          val v = new Variable[Post](t)(varO)
          cNameSuccessor(RefCLocalDeclaration(decl, idx)) = v
          implicit val o: Origin = init.o
          init.init
            .map(value => Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value)))))
            .getOrElse(LocalDecl(v))
      }
    })(decl.o)
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
    // TODO: create requirement that shared memory arrays are not NULL?
    if(!globalFence || !localFence){
      val redist = permissionScanner(barrier)
      if(!globalFence)
        redist
        .intersect(globalMemNames.toSet)
        .foreach(v => throw RedistributingBarrier(v, global = true))
      if(!localFence)
        redist
          .intersect(dynamicSharedMemNames.union(dynamicSharedMemNames).toSet)
          .foreach(v => throw RedistributingBarrier(v, global = false))
    }

    ParBarrier[Post](
      block = cudaCurrentBlock.top.ref,
      invs = Nil,
      requires = rw.dispatch(barrier.requires),
      ensures = rw.dispatch(barrier.ensures),
      content = Block(Nil),
    )(PanicBlame("more panic"))
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
      case e: Expr[Pre] if e.t != TResource[Pre]() => return Seq()
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
      case ("get_local_id", Some(i)) => cudaCurrentThreadIdx.top.indices.values.toSeq.apply(i).get
      case ("get_group_id", Some(i)) => cudaCurrentBlockIdx.top.indices.values.toSeq.apply(i).get
      case ("get_local_size", Some(i)) => cudaCurrentBlockDim.top.indices.values.toSeq.apply(i).get
      case ("get_num_groups", Some(i)) => cudaCurrentGridDim.top.indices.values.toSeq.apply(i).get
      case _ => ProcedureInvocation[Post](cFunctionDeclSuccessor.ref((decls, initIdx)), args.map(rw.dispatch), Nil, Nil,
        givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
        yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
    }
  }
}

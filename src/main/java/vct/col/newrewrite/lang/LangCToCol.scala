package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin.{AbstractApplicable, InterpretedOriginVariable, Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, C, CNameTarget, RefADTFunction, RefAxiomaticDataType, RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCudaBlockDim, RefCudaBlockIdx, RefCudaGridDim, RefCudaThreadIdx, RefCudaVec, RefCudaVecDim, RefCudaVecX, RefCudaVecY, RefCudaVecZ, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecInvocationTarget}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.parsers.Language
import vct.result.VerificationError.UserError

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
  private var inKernelArgs: Boolean = false

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
    node match {
      // SharedMemSize gets rewritten towards the length of a shared memory name, so is valid in global context
      case _: SharedMemSize[Pre] =>
      case l: CLocal[Pre] => l.ref match {
        case Some(ref: RefCParam[Pre])
          if dynamicSharedMemNames.contains(ref) || staticSharedMemNames.contains(ref) => return false
        case None => if(!allowedNonRefs.contains(l.name)) ???
        case _ =>
      }
      case e => if(!e.subnodes.forall(hasNoSharedMemNames)) return false
    }
    true
  }

  def rewriteUnit(cUnit: CTranslationUnit[Pre]): Unit = {
    cUnit.declarations.foreach(rw.dispatch)
  }

  def cDeclToName(cDecl: CDeclarator[Pre]): String = cDecl match {
    case CPointerDeclarator(_, inner) => cDeclToName(inner)
    case CArrayDeclarator(_, _, inner) => cDeclToName(inner)
    case CTypedFunctionDeclarator(_, _, inner) => cDeclToName(inner)
    case CAnonymousFunctionDeclarator(_, inner) => cDeclToName(inner)
    case CName(name: String) => name
  }

  def sharedSize(shared: SharedMemSize[Pre]): Expr[Post] = {
    val SharedMemSize(pointer) = shared

    pointer match {
      case loc: CLocal[Pre] => Local[Post](dynamicSharedMemLengthVar(loc.ref.get).ref)(shared.o)
      case _ => ???
    }
  }

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    cParam.drop()
    val o = InterpretedOriginVariable(cDeclToName(cParam.declarator), cParam.o)

    var array = false
    var pointer = false
    var global = false
    var shared = false
    var extern = false
    var innerType: Option[Type[Pre]] = None

    var isDynamicShared = false

    val cRef = RefCParam(cParam)

    cParam.specifiers.foreach{
      case GPULocal() => shared = true
      case GPUGlobal() => global = true
      case CSpecificationType(TPointer(t)) =>
        pointer = true
        innerType = Some(t)
      case CSpecificationType(TArray(t)) =>
        array = true
        innerType = Some(t)
      case CExtern() => extern = true
      case _ =>
    }

    if((shared || global) && !inKernel) throw WrongGPUType(cParam)
    if(inKernelArgs){
      language.get match {
        case Language.C =>
          if(global && !shared && (pointer || array) && !extern) globalMemNames.add(cRef)
          else if(shared && !global && (pointer || array) && !extern) {
            dynamicSharedMemNames.add(cRef)
            // Create Var with array type here and return
            isDynamicShared = true
            val v = new Variable[Post](TArray[Post]( rw.dispatch(innerType.get) )(o) )(o)
            cNameSuccessor(cRef) = v
            return
          }
          else if(!shared && !global && !pointer && !array && !extern) ()
          else throw WrongGPUKernelParameterType(cParam)
        case Language.CUDA =>
          if(!global && !shared && (pointer || array) && !extern) globalMemNames.add(cRef)
          else if(!shared && !global && !pointer && !array && !extern) ()
          else throw WrongGPUKernelParameterType(cParam)
        case _ => ??? // Should not happen
      }
    }

    val v = new Variable[Post](cParam.specifiers.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???))(o)
    cNameSuccessor(cRef) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    if (func.specs.collectFirst { case CKernel() => () }.nonEmpty){
      inKernel = true
      inKernelArgs = true
    }
    val params = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
    inKernel = false
    inKernelArgs = false

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
            val namedO = InterpretedOriginVariable(cDeclToName(func.declarator), func.o)
            kernelProcedure(namedO, contract, info, Some(func.body))
          } else {
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
    val filteredIdx = idx.indices.values.zip(dim.indices.values).filter{ case (i, _) => vars.contains(i)}
    val otherIdx = idx.indices.values.zip(dim.indices.values).filterNot{ case (i, _) => vars.contains(i)}

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
      case _ => e.subnodes.foldLeft(vars)( (set, node) => set ++ findVars(node) )
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
        inKernelArgs = true
        val args = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
        inKernelArgs = false
        rw.variables.collect { dynamicSharedMemNames.foreach(d => rw.variables.declare(cNameSuccessor(d)) ) }
        val (sharedMemSizes, sharedMemInit: Seq[Statement[Post]]) = rw.variables.collect {
          var result: Seq[Statement[Post]] = Seq()
          dynamicSharedMemNames.foreach(d =>
          {
            implicit val o: Origin = d.decl.o
            val v = new Variable[Post](TInt())
            dynamicSharedMemLengthVar(d) = v
            rw.variables.declare(v)
            val decl: Statement[Post] = LocalDecl(cNameSuccessor(d))
            val assign: Statement[Post] = Assign[Post](Local(cNameSuccessor(d).ref)
              , NewArray[Post](v.t, Seq(Local(v.ref)), 0))(PanicBlame("Assign should work"))
            result = result ++  Seq(decl, assign)
          })
          result
        }

        val newArgs = blockDim.indices.values.toSeq ++ gridDim.indices.values.toSeq ++ args ++ sharedMemSizes

        val newGivenArgs = rw.variables.dispatch(contract.givenArgs)
        val newYieldsArgs = rw.variables.dispatch(contract.yieldsArgs)
        // We add the requirement that a GPU kernel must always have threads (non zero block or grid dimensions)
        val nonZeroThreadsSeq: Seq[Expr[Post]]
          = (blockDim.indices.values ++ gridDim.indices.values).map( v => Less(IntegerValue(0)(o), v.get(o))(o) ).toSeq
        val nonZeroThreads = foldStar(nonZeroThreadsSeq)(o)

        val nonZeroThreadsPred =
          nonZeroThreadsSeq
            .map(UnitAccountedPredicate(_)(o))
            .reduceLeft[AccountedPredicate[Post]](SplitAccountedPredicate(_,_)(o))

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
                      requires = rw.dispatch(foldStar(unfoldPredicate(contract.requires))),
                      ensures = rw.dispatch(foldStar(unfoldPredicate(contract.ensures))),
                      content = rw.dispatch(impl),
                    )(PanicBlame("where blame?")))
                  ParStatement(ParBlock(
                    decl = gridDecl,
                    iters = blockIdx.indices.values.zip(gridDim.indices.values).map {
                      case (index, dim) => IterVariable(index, const(0), dim.get)
                    }.toSeq,
                    // Context is added to requires and ensures here
                    context_everywhere = tt,
                    requires = Star(
                        Star(nonZeroThreads,
                          foldStar(
                            unfoldPredicate(contract.requires)
                              .filter(hasNoSharedMemNames)
                              .map(allThreadsInBlock)))
                        , contextBlock),
                    ensures = Star(
                      Star(nonZeroThreads,
                        foldStar(
                          unfoldPredicate(contract.ensures)
                            .filter(hasNoSharedMemNames)
                            .map(allThreadsInBlock)) )
                      , contextBlock),
                    // Add shared memory initialization before beginning of inner parallel block
                    content = Block[Post](sharedMemInit ++ Seq(innerContent))
                  )(PanicBlame("where blame?")))
                }
              }
            }
          }
        })

        val gridContext: AccountedPredicate[Post] =
          foldPredicate(unfoldStar(contract.contextEverywhere)
            .filter(hasNoSharedMemNames)
            .map(allThreadsInBlock))(o)
        new Procedure[Post](
          returnType = TVoid(),
          args = newArgs,
          outArgs = Nil, typeArgs = Nil,
          body = parBody,
          contract = ApplicableContract(
            SplitAccountedPredicate(
              SplitAccountedPredicate(nonZeroThreadsPred,
                mapPredicate(filterPredicate(contract.requires, hasNoSharedMemNames), allThreadsInGrid))(o),
              gridContext)(o),
            SplitAccountedPredicate(
              SplitAccountedPredicate(nonZeroThreadsPred,
                mapPredicate(filterPredicate(contract.ensures, hasNoSharedMemNames), allThreadsInGrid))(o),
              gridContext)(o),
            // Context everywhere is already passed down in the body
            tt,
            contract.signals.map(rw.dispatch),
            newGivenArgs,
            newYieldsArgs,
            contract.decreases.map(rw.dispatch),
          )(contract.blame)(contract.o)
        )(AbstractApplicable)(o)
      }
    }
  }

  def rewriteGlobalDecl(decl: CGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    for((init, idx) <- decl.decl.inits.zipWithIndex) {
      if(init.ref.isEmpty) {
        // Otherwise, skip the declaration: the definition is used instead.
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
          val v = new Variable[Post](t)(init.o)
          cNameSuccessor(RefCLocalDeclaration(decl, idx)) = v
          implicit val o: Origin = init.o
          init.init match {
            case Some(value) =>
              Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value))))
            case None => LocalDecl(v)
          }
      }
    })(decl.o)
  }

  def rewriteGoto(goto: CGoto[Pre]): Statement[Post] =
    Goto[Post](rw.succ(goto.ref.getOrElse(???)))(goto.o)

  def localBarrier(barrier: GpgpuLocalBarrier[Pre]): Statement[Post] = {
    implicit val o: Origin = barrier.o
    ParBarrier[Post](
      block = cudaCurrentBlock.top.ref,
      invs = Nil,
      requires = rw.dispatch(barrier.requires),
      ensures = rw.dispatch(barrier.ensures),
      content = Block(Nil),
    )(PanicBlame("more panic"))
  }

  def globalBarrier(barrier: GpgpuGlobalBarrier[Pre]): Statement[Post] = {
    implicit val o: Origin = barrier.o
    ParBarrier[Post](
      block = cudaCurrentGrid.top.ref,
      invs = Nil,
      requires = rw.dispatch(barrier.requires),
      ensures = rw.dispatch(barrier.ensures),
      content = Block(Nil),
    )(PanicBlame("more panic"))
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
    deref.ref.get match {
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(deref.blame)
      case BuiltinField(f) => rw.dispatch(f(deref.struct))
      case target: SpecInvocationTarget[Pre] => ???
      case dim: RefCudaVecDim[Pre] => dim.vec match {
        case RefCudaThreadIdx() => cudaCurrentThreadIdx.top.indices(dim).get
        case RefCudaBlockIdx() => cudaCurrentBlockIdx.top.indices(dim).get
        case RefCudaBlockDim() => cudaCurrentBlockDim.top.indices(dim).get
        case RefCudaGridDim() => cudaCurrentGridDim.top.indices(dim).get
      }
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
      case e@ RefCGlobalDeclaration(decls, initIdx) =>
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
}

package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin.{AbstractApplicable, Blame, CallableFailure, Origin, PanicBlame, TrueSatisfiable}
import vct.col.ref.Ref
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, C, CInvocationTarget, CNameTarget, RefADTFunction, RefAxiomaticDataType, RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCudaBlockDim, RefCudaBlockIdx, RefCudaGridDim, RefCudaThreadIdx, RefCudaVec, RefCudaVecDim, RefCudaVecX, RefCudaVecY, RefCudaVecZ, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable, SpecDerefTarget, SpecInvocationTarget}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.{Substitute, SuccessionMap}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

import scala.collection.immutable.ListMap

case object LangCToCol {
  case class CGlobalStateNotSupported(example: CInit[_]) extends UserError {
    override def code: String = "notSupported"
    override def text: String =
      example.o.messageInContext("Global variables in C are not supported.")
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

  def rewriteUnit(cUnit: CTranslationUnit[Pre]): Unit = {
    cUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    cParam.drop()
    val v = new Variable[Post](cParam.specifiers.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???))(cParam.o)
    cNameSuccessor(RefCParam(cParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    val params = rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1

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
            kernelProcedure(func.o, contract, info, Some(func.body))
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
    implicit val o: Origin = e.o
    Starall(idx.indices.values.toSeq, Nil, Implies(
      foldAnd(idx.indices.values.zip(dim.indices.values).map {
        case (idx, dim) => const[Post](0) <= idx.get && idx.get < dim.get
      }),
      e
    ))(PanicBlame("Where blame"))
  }

  def allThreadsInBlock(e: Expr[Pre]): Expr[Post] = {
    val thread = new CudaVec(RefCudaThreadIdx())(e.o)
    cudaCurrentThreadIdx.having(thread) { all(thread, cudaCurrentBlockDim.top, rw.dispatch(e)) }
  }

  def allThreadsInGrid(e: Expr[Pre]): Expr[Post] = {
    val thread = new CudaVec(RefCudaThreadIdx())(e.o)
    val block = new CudaVec(RefCudaBlockIdx())(e.o)
    cudaCurrentBlockIdx.having(block) { all(block, cudaCurrentGridDim.top, allThreadsInBlock(e)) }
  }

  def kernelProcedure(o: Origin, contract: ApplicableContract[Pre], info: C.DeclaratorInfo[Pre], body: Option[Statement[Pre]]): Procedure[Post] = {
    val blockDim = new CudaVec(RefCudaBlockDim())(o)
    val gridDim = new CudaVec(RefCudaGridDim())(o)
    cudaCurrentBlockDim.having(blockDim) {
      cudaCurrentGridDim.having(gridDim) {
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
                  ParStatement(ParBlock(
                    decl = gridDecl,
                    iters = blockIdx.indices.values.zip(gridDim.indices.values).map {
                      case (index, dim) => IterVariable(index, const(0), dim.get)
                    }.toSeq,
                    context_everywhere = allThreadsInBlock(contract.contextEverywhere),
                    requires = allThreadsInBlock(foldStar(unfoldPredicate(contract.requires))),
                    ensures = allThreadsInBlock(foldStar(unfoldPredicate(contract.ensures))),
                    content = ParStatement(ParBlock(
                      decl = blockDecl,
                      iters = threadIdx.indices.values.zip(blockDim.indices.values).map {
                        case (index, dim) => IterVariable(index, const(0), dim.get)
                      }.toSeq,
                      context_everywhere = rw.dispatch(contract.contextEverywhere),
                      requires = rw.dispatch(foldStar(unfoldPredicate(contract.requires))),
                      ensures = rw.dispatch(foldStar(unfoldPredicate(contract.ensures))),
                      content = rw.dispatch(impl),
                    )(PanicBlame("where blame?")))
                  )(PanicBlame("where blame?")))
                }
              }
            }
          }
        })

        new Procedure[Post](
          returnType = TVoid(),
          args = blockDim.indices.values.toSeq ++ gridDim.indices.values.toSeq ++ rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1,
          outArgs = Nil, typeArgs = Nil,
          body = parBody,
          contract = ApplicableContract(
            mapPredicate(contract.requires, allThreadsInGrid),
            mapPredicate(contract.ensures, allThreadsInGrid),
            allThreadsInGrid(contract.contextEverywhere),
            contract.signals.map(rw.dispatch),
            rw.variables.dispatch(contract.givenArgs),
            rw.variables.dispatch(contract.yieldsArgs),
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
      case RefAxiomaticDataType(decl) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCParam[Pre] =>
        if(cCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cNameSuccessor.ref(RefCParam(cCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cNameSuccessor.ref(ref))
      case RefCFunctionDefinition(decl) => throw NotAValue(local)
      case RefCGlobalDeclaration(decls, initIdx) => throw NotAValue(local)
      case ref: RefCLocalDeclaration[Pre] => Local(cNameSuccessor.ref(ref))
      case ref: RefCudaVec[Pre] => throw NotAValue(local)
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
      case RefCGlobalDeclaration(decls, initIdx) =>
        ProcedureInvocation[Post](cFunctionDeclSuccessor.ref((decls, initIdx)), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
    }
  }
}

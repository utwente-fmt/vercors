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

  val cudaCurrentIndexVariables: ScopedStack[CudaIndexVariables] = ScopedStack()
  val cudaCurrentDimensionVariables: ScopedStack[CudaDimensionVariables] = ScopedStack()

  case class CudaIndexVariableOrigin(dim: RefCudaVecDim[_]) extends Origin {
    override def preferredName: String =
      dim.vec.name + dim.name.toUpperCase

    override def context: String = s"At: [Variable for dimension ${dim.name} of ${dim.vec.name}]"
    override def inlineContext: String = s"[Variable for dimension ${dim.name} of ${dim.vec.name}]"
    override def shortPosition: String = "generated"
  }

  class CudaIndexVariables(implicit val o: Origin) {
    val indices: ListMap[RefCudaVecDim[Pre], Variable[Post]] =
      ListMap((for(
        set <- Seq(RefCudaThreadIdx[Pre](), RefCudaBlockIdx[Pre]());
        dim <- Seq(RefCudaVecX[Pre](_), RefCudaVecY[Pre](_), RefCudaVecZ[Pre](_))
      ) yield dim(set) -> new Variable[Post](TInt())(CudaIndexVariableOrigin(dim(set)))): _*)
  }

  class CudaDimensionVariables(implicit val o: Origin) {
    val indices: ListMap[RefCudaVecDim[Pre], Variable[Post]] =
      ListMap((for (
        set <- Seq(RefCudaBlockDim[Pre](), RefCudaGridDim[Pre]());
        dim <- Seq(RefCudaVecX[Pre](_), RefCudaVecY[Pre](_), RefCudaVecZ[Pre](_))
      ) yield dim(set) -> new Variable[Post](TInt())(CudaIndexVariableOrigin(dim(set)))): _*)
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
            kernelProcedure(func.o, contract, info)
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

  def indexed(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    val indices = new CudaIndexVariables()
    Starall(indices.indices.values.toSeq, Nil, Implies(
      foldAnd(indices.indices.values.zip(cudaCurrentDimensionVariables.top.indices.values).map {
        case (index, dim) => const[Post](0) <= index.get && index.get < dim.get
      }),
      cudaCurrentIndexVariables.having(indices) { rw.dispatch(e) },
    ))(PanicBlame("Where blame")) // TODO figure out where to put blame
  }

  def indexed(p: AccountedPredicate[Pre]): AccountedPredicate[Post] = p match {
    case UnitAccountedPredicate(pred) =>
      UnitAccountedPredicate(indexed(pred))(p.o)
    case SplitAccountedPredicate(left, right) =>
      SplitAccountedPredicate(indexed(left), indexed(right))(p.o)
  }

  def kernelProcedure(o: Origin, contract: ApplicableContract[Pre], info: C.DeclaratorInfo[Pre]): Procedure[Post] = {
    val dims = new CudaDimensionVariables()(o)
    cudaCurrentDimensionVariables.having(dims) {
      new Procedure[Post](
        returnType = TVoid(),
        args = dims.indices.values.toSeq ++ rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1,
        outArgs = Nil, typeArgs = Nil,
        body = None,
        contract = ApplicableContract(
          indexed(contract.requires),
          indexed(contract.ensures),
          indexed(contract.contextEverywhere),
          contract.signals.map(rw.dispatch),
          rw.variables.dispatch(contract.givenArgs),
          rw.variables.dispatch(contract.yieldsArgs),
          contract.decreases.map(rw.dispatch),
        )(contract.blame)(contract.o)
      )(AbstractApplicable)(o)
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
                kernelProcedure(init.o, decl.decl.contract, info)
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
        case RefCudaThreadIdx() | RefCudaBlockIdx() =>
          cudaCurrentIndexVariables.top.indices(dim).get
        case RefCudaBlockDim() | RefCudaGridDim() =>
          cudaCurrentDimensionVariables.top.indices(dim).get
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

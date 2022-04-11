package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.newrewrite.lang.LangSpecificToCol.NotAValue
import vct.col.origin.{AbstractApplicable, Origin}
import vct.col.ref.Ref
import vct.col.resolve.{BuiltinInstanceMethod, C, CInvocationTarget, CNameTarget, RefADTFunction, RefAxiomaticDataType, RefCDeclaration, RefCFunctionDefinition, RefCGlobalDeclaration, RefCParam, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure, RefVariable}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object LangCToCol {
  case class CGlobalStateNotSupported(example: CInit[_]) extends UserError {
    override def code: String = "notSupported"
    override def text: String =
      example.o.messageInContext("Global variables in C are not supported.")
  }
}

case class LangCToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangCToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val cFunctionSuccessor: SuccessionMap[CInvocationTarget[Pre], Procedure[Post]] = SuccessionMap()
  val cNameSuccessor: SuccessionMap[CNameTarget[Pre], Variable[Post]] = SuccessionMap()

  def rewriteParam(cParam: CParam[Pre]): Unit = {
    cParam.drop()
    val v = new Variable[Post](cParam.specifiers.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???))(cParam.o)
    cNameSuccessor(RefCParam(cParam)) = v
    v.declareDefault(rw)
  }

  def rewriteFunctionDef(func: CFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = C.getDeclaratorInfo(func.declarator)
    val returnType = func.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    val params = rw.collectInScope(rw.variableScopes) { info.params.get.foreach(rw.dispatch) }
    cFunctionSuccessor(RefCFunctionDefinition(func)) = new Procedure(
      returnType = returnType,
      args = params,
      outArgs = Nil,
      typeArgs = Nil,
      body = Some(rw.dispatch(func.body)),
      contract = contract()(func.o),
    )(func.blame)(func.o).declareDefault(rw)
  }

  def rewriteGlobalDecl(decl: CGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    for((init, idx) <- decl.decl.inits.zipWithIndex) {
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) =>
          cFunctionSuccessor(RefCGlobalDeclaration(decl, idx)) = new Procedure[Post](
            returnType = t,
            args = rw.collectInScope(rw.variableScopes) { params.foreach(rw.dispatch) },
            outArgs = Nil,
            typeArgs = Nil,
            body = None,
            contract = contract()(init.o),
          )(AbstractApplicable)(init.o).declareDefault(rw)
        case None =>
          throw CGlobalStateNotSupported(init)
      }
    }
  }

  def rewriteLocal(decl: CDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    // PB: this is correct because Seq[CInit]'s are flattened, but the structure is a bit stupid.
    val t = decl.specs.collectFirst { case t: CSpecificationType[Pre] => rw.dispatch(t.t) }.getOrElse(???)
    Block(for((init, idx) <- decl.inits.zipWithIndex) yield {
      val info = C.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) => ???
        case None =>
          val v = new Variable[Post](t)(init.o)
          cNameSuccessor(RefCDeclaration(decl, idx)) = v
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
    Result[Post](cFunctionSuccessor.ref(ref))

  def result(ref: RefCGlobalDeclaration[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cFunctionSuccessor.ref(ref))

  def local(local: CLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case RefAxiomaticDataType(decl) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCParam[Pre] => Local(cNameSuccessor.ref(ref))
      case RefCFunctionDefinition(decl) => throw NotAValue(local)
      case RefCGlobalDeclaration(decls, initIdx) => throw NotAValue(local)
      case ref: RefCDeclaration[Pre] => Local(cNameSuccessor.ref(ref))
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
        ProcedureInvocation[Post](cFunctionSuccessor.ref(ref), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (rw.succ(e), rw.succ(v)) })(inv.blame)
      case RefCGlobalDeclaration(decls, initIdx) => ???
      case RefCDeclaration(decls, initIdx) => ???
    }
  }
}

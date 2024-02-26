package vct.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{PVLInvocation, _}
import vct.col.origin.{Origin, PanicBlame, PostBlameSplit, TrueSatisfiable}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.rewrite.lang.LangSpecificToCol.{NotAValue, ThisVar}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{BuiltinField, BuiltinInstanceMethod, ImplicitDefaultPVLConstructor, PVLBuiltinInstanceMethod, RefADTFunction, RefAxiomaticDataType, RefClass, RefEndpoint, RefEnum, RefEnumConstant, RefField, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModel, RefModelAction, RefModelField, RefModelProcess, RefPVLConstructor, RefPVLEndpoint, RefPredicate, RefProcedure, RefProverFunction, RefVariable, SpecDerefTarget, SpecInvocationTarget, SpecNameTarget}
import vct.col.util.{AstBuildHelpers, SuccessionMap}
import vct.col.resolve.ctx.{ImplicitDefaultPVLConstructor, PVLConstructorTarget}
import vct.rewrite.lang.LangPVLToCol.ModelConstructorNotSupported
import vct.result.VerificationError.SystemError

case object LangPVLToCol {
  case class ModelConstructorNotSupported() extends SystemError {
    override def text: String = "VerCors attempted to get the constructor of a model, but this is not yet supported"
  }
}

case class LangPVLToCol[Pre <: Generation](rw: LangSpecificToCol[Pre], veymontGeneratePermissions: Boolean) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val pvlDefaultConstructor: SuccessionMap[Class[Pre], Constructor[Post]] = SuccessionMap()
  val pvlConstructor: SuccessionMap[PVLConstructor[Pre], Constructor[Post]] = SuccessionMap()

  def constructorSucc(ref: PVLConstructorTarget[Pre]): Ref[Post, Constructor[Post]] = ref match {
    case ImplicitDefaultPVLConstructor(cls) => pvlDefaultConstructor.ref(cls)
    case RefPVLConstructor(cons) => pvlConstructor.ref(cons)
    case RefModel(_) => throw new ModelConstructorNotSupported()
  }

  def rewriteConstructor(cons: PVLConstructor[Pre]): Unit = {
    implicit val o: Origin = cons.o
    val t = TClass[Post](rw.succ(rw.currentClass.top))
    val resVar = new Variable(t)
    pvlConstructor(cons) =
      rw.currentThis.having(ThisObject(rw.succ(rw.currentClass.top))) {
        rw.classDeclarations.declare(new Constructor[Post](
          cls = rw.succ(rw.currentClass.top),
          args = rw.variables.dispatch(cons.args),
          outArgs = Nil,
          typeArgs = Nil,
          body = cons.body.map(rw.dispatch),
          contract = rw.dispatch(cons.contract),
        )(cons.blame)(cons.o.where(name = s"constructor${rw.currentClass.top.o.getPreferredNameOrElse().ucamel}")))
      }
  }

  def maybeDeclareDefaultConstructor(cls: Class[Pre]): Unit = {
    if (cls.declarations.collectFirst { case _: PVLConstructor[Pre] => () }.isEmpty) {
      implicit val o: Origin = cls.o
      val t = TClass[Post](rw.succ(cls))
      val `this` = ThisObject(rw.succ[Class[Post]](cls))
      val defaultBlame = PanicBlame("The postcondition of a default constructor cannot fail.")

      val checkRunnable = cls.declarations.collectFirst {
        case _: RunMethod[Pre] => ()
      }.nonEmpty

      pvlDefaultConstructor(cls) = rw.classDeclarations.declare(new Constructor[Post](
        rw.succ(cls),
        Nil, Nil, Nil,
        Some(Scope(Nil, Block(Nil))),
        ApplicableContract(
          UnitAccountedPredicate(tt),
          UnitAccountedPredicate(AstBuildHelpers.foldStar(cls.declarations.collect {
            case field: InstanceField[Pre] if field.flags.collectFirst { case _: Final[Pre] => () }.isEmpty && !veymontGeneratePermissions =>
              fieldPerm[Post](`this`, rw.succ(field), WritePerm())
          }) &* (if (checkRunnable) IdleToken(`this`) else tt)), tt, Nil, Nil, Nil, None,
        )(TrueSatisfiable)
      )(defaultBlame))
    }
  }

  def local(local: PVLLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o

    local.ref.get match {
      case spec: SpecNameTarget[Pre] => rw.specLocal(spec, local, local.blame)
      case RefField(decl) => Deref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case endpoint: RefPVLEndpoint[Pre] => rw.veymont.rewriteEndpointUse(endpoint, local)
    }
  }

  def deref(deref: PVLDeref[Pre]): Expr[Post] = {
    implicit val o: Origin = deref.o
    deref.ref.get match {
      case spec: SpecDerefTarget[Pre] => rw.specDeref(deref.obj, spec, deref, deref.blame)
      case RefField(decl) => Deref[Post](rw.dispatch(deref.obj), rw.succ(decl))(deref.blame)
    }
  }

  def invocation(inv: PVLInvocation[Pre]): Expr[Post] = {
    val PVLInvocation(obj, _, args, typeArgs, givenMap, yields) = inv
    implicit val o: Origin = inv.o

    inv.ref.get match {
      case spec: SpecInvocationTarget[Pre] =>
        rw.specInvocation(inv.obj, spec, typeArgs, args, givenMap, yields, inv, inv.blame)
      case PVLBuiltinInstanceMethod(f) =>
        rw.dispatch(f(obj.get)(args))
    }
  }

  def newClass(inv: PVLNew[Pre]): Expr[Post] = {
    val PVLNew(t, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefModel(decl) => ModelNew[Post](rw.succ(decl))
      case RefPVLConstructor(decl) =>
        ConstructorInvocation[Post](pvlConstructor.ref(decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case ImplicitDefaultPVLConstructor(_) =>
        ConstructorInvocation[Post](pvlDefaultConstructor.ref(t.asInstanceOf[TClass[Pre]].cls.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
    }
  }

  def branch(branch: PVLBranch[Pre]): Statement[Post] =
    if (rw.veymont.currentProg.nonEmpty) {
      rw.veymont.rewriteBranch(branch)
    } else {
      Branch(branch.branches.map { case (e, s) => (rw.dispatch(e), rw.dispatch(s)) })(branch.o)
    }

  def loop(loop: PVLLoop[Pre]): Statement[Post] = loop match {
    case PVLLoop(Block(Nil), _, Block(Nil), _, _) if rw.veymont.currentProg.nonEmpty =>
      rw.veymont.rewriteLoop(loop)
    case PVLLoop(init, cond, update, contract, body) =>
      Loop(rw.dispatch(init), rw.dispatch(cond), rw.dispatch(update), rw.dispatch(contract), rw.dispatch(body))(loop.o)
  }

  def assign(assign: Assign[Pre]): Statement[Post] =
    if (rw.veymont.currentProg.nonEmpty)
      rw.veymont.rewriteAssign(assign)
    else
      assign.rewriteDefault()

}

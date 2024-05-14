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
    if (cls.decls.collectFirst { case _: PVLConstructor[Pre] => () }.isEmpty) {
      implicit val o: Origin = cls.o
      val `this` = ThisObject(rw.succ[Class[Post]](cls))
      val defaultBlame = PanicBlame("The postcondition of a default constructor cannot fail.")

      val checkRunnable = cls.decls.collectFirst {
        case _: RunMethod[Pre] => ()
      }.nonEmpty

      pvlDefaultConstructor(cls) = rw.classDeclarations.declare(new Constructor[Post](
        rw.succ(cls),
        Nil, Nil, Nil,
        Some(Scope(Nil, Block(Nil))),
        ApplicableContract(
          UnitAccountedPredicate(tt),
          UnitAccountedPredicate(AstBuildHelpers.foldStar(cls.decls.collect {
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
      case endpoint: RefPVLEndpoint[Pre] => EndpointNameExpr(rw.veymont.rewriteEndpointUse(endpoint, local))
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
    val PVLNew(t, typeArgs, args, givenMap, yields) = inv
    val classTypeArgs = t match {
      case TClass(_, typeArgs) => typeArgs
      case _ => Seq()
    }
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefModel(decl) => ModelNew[Post](rw.succ(decl))
      case RefPVLConstructor(decl) =>
        ConstructorInvocation[Post](pvlConstructor.ref(decl), classTypeArgs.map(rw.dispatch), args.map(rw.dispatch),
          Nil, typeArgs.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case ImplicitDefaultPVLConstructor(_) =>
        ConstructorInvocation[Post](pvlDefaultConstructor.ref(t.asInstanceOf[TClass[Pre]].cls.decl), classTypeArgs.map(rw.dispatch),
          args.map(rw.dispatch), Nil, typeArgs.map(rw.dispatch),
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
    }
  }

  def branch(branch: PVLBranch[Pre]): Statement[Post] =
    Branch(branch.branches.map { case (e, s) => (rw.dispatch(e), rw.dispatch(s)) })(branch.o)

  def loop(loop: PVLLoop[Pre]): Statement[Post] = loop match {
    case PVLLoop(init, cond, update, contract, body) =>
      Loop(rw.dispatch(init), rw.dispatch(cond), rw.dispatch(update), rw.dispatch(contract), rw.dispatch(body))(loop.o)
  }

  def rewriteMainMethod(main: VeSUVMainMethod[Pre]): Unit = {
    implicit val o: Origin = main.o
    main.drop()
    val body: Option[Statement[Post]] = main.body match {
      case None => None
      case Some(s) => Some(s.rewriteDefault())
    }
    val empty_pred: AccountedPredicate[Post] = UnitAccountedPredicate(BooleanValue(value = true))
    // TODO: Where does the blame come from?
    val contract: ApplicableContract[Post] = ApplicableContract(empty_pred, empty_pred, BooleanValue(value = true), Seq(), Seq(), Seq(), None)(o)
    val new_main: Procedure[Post] = new Procedure(TVoid(), Seq(), Seq(), Seq(), body, contract, false, false, true)(main.blame)
    new_main.declare()
  }
}

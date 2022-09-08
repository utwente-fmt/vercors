package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{AbstractRewriter, BipComponent, BipData, BipGuard, BipIncomingData, BipLocalIncomingData, BipOutgoingData, BipStatePredicate, BipTransition, Expr, InstanceMethod, InvokeMethod, JavaAnnotation, JavaClass, JavaLocal, JavaMethod, JavaParam, MethodInvocation, PinnedDecl, Procedure, TBool, TVoid, Type, Variable}
import vct.col.resolve.{JavaAnnotationData => jad}
import vct.col.newrewrite.lang.LangBipToCol.{InconsistentBipDataType, TodoError, WrongGuardReturnType, WrongTransitionReturnType}
import vct.col.origin.{BipComponentInvariantNotMaintained, BipGuardInvocationFailure, BipStateInvariantNotMaintained, BipTransitionFailure, BipTransitionPostconditionFailure, Blame, CallableFailure, DiagnosticOrigin, Origin, PanicBlame, SourceNameOrigin}
import vct.col.ref.Ref
import vct.col.resolve.{ImplicitDefaultJavaBipStatePredicate, Java, JavaBipStatePredicateTarget, RefJavaBipStatePredicate}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.immutable.ListMap
import scala.collection.mutable

case object LangBipToCol {
  case class TodoError() extends UserError {
    override def code: String = ???
    override def text: String = ???
  }

  case class WrongTransitionReturnType(m: JavaMethod[_]) extends UserError {
    override def code: String = "bipWrongTransitionReturnType"
    override def text: String = m.o.messageInContext(s"The return type of this update function should be void, instead of ${m.returnType}")
  }

  case class WrongGuardReturnType(m: JavaMethod[_]) extends UserError {
    override def code: String = "bipWrongGuardReturnType"
    override def text: String = m.o.messageInContext(s"The return type of this guard should be boolean, instead of ${m.returnType}")
  }

  case class InconsistentBipDataType(data: BipData[_], expectedOrigin: Origin, expectedType: Type[_]) extends UserError {
    override def code: String = "bipInconsistentDataType"
    override def text: String = ???
  }
}

// TODO (RR): More specific origins (e.g. avoid SourceNameOrigin), errors, panicBlame...

case class LangBipToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val defaultBipStatePredicates: mutable.Map[String, BipStatePredicate[Post]] = mutable.Map()
  val bipStatePredicates: SuccessionMap[jad.BipStatePredicate[Pre], BipStatePredicate[Post]] = SuccessionMap()
  var bipDatas: ListMap[String, (Type[Pre], BipData[Post])] = ListMap()

  def getJavaBipStatePredicate(t: JavaBipStatePredicateTarget[Pre]): Ref[Post, BipStatePredicate[Post]] = t match {
    case RefJavaBipStatePredicate(decl) => bipStatePredicates.ref(decl.data.get.asInstanceOf[jad.BipStatePredicate[Pre]])
    case ImplicitDefaultJavaBipStatePredicate(state) => defaultBipStatePredicates.getOrElseUpdate(state, {
      val bsp = new BipStatePredicate[Post](tt)(DiagnosticOrigin)
      bsp.declareDefault(rw)
      bsp
    }).ref
  }

  def getBipData(name: String, expectedType: Type[Pre])(implicit o: Origin): Ref[Post, BipData[Post]] = {
    bipDatas.get(name) match {
      case Some((preType, data)) =>
        // No subtyping (yet): references of a data must all have exact same type
        if (preType != expectedType) {
          throw InconsistentBipDataType(data, o, expectedType)
        } else {
          data.ref
        }
      case None =>
        val data = new BipData[Post](rw.dispatch(expectedType)).declareDefault(rw)
        bipDatas = bipDatas.updated(name, (expectedType, data))
        data.ref
    }
  }

  def rewriteParameter(p: JavaParam[Pre]): Unit = {
    val jad.BipData(name) = jad.BipData.get(p).get
    new BipIncomingData(getBipData(name, p.t)(p.o))(p.o).succeedDefault(p)
  }

  def rewriteTransition(m: JavaMethod[Pre]): Unit = {
    val jad.BipTransition(_, source, target, guard, requires, ensures) = jad.BipTransition.get(m).get

    if (m.returnType != TVoid[Pre]()) { throw WrongTransitionReturnType(m) }

    val trans = new BipTransition[Post](
      getJavaBipStatePredicate(source),
      getJavaBipStatePredicate(target),
      rw.collectInScope(rw.bipIncomingDataScopes) { m.parameters.map(rewriteParameter) },
      guard.map(rw.succ(_)),
      rw.dispatch(requires),
      rw.dispatch(ensures),
      rw.dispatch(m.body.get))(m.blame)(SourceNameOrigin(m.name, m.o))

    trans.succeedDefault(m)(rw)
  }

  def local(local: JavaLocal[Pre], decl: JavaParam[Pre]): Expr[Post] = {
    val jad.BipData(_) = jad.BipData.get(decl).get
    BipLocalIncomingData(rw.succ[BipIncomingData[Post]](decl))(local.o)
  }

  def rewriteGuard(m: JavaMethod[Pre]): Unit = {
    // TODO (RR): Add ensures to @Guard
    // TODO (RR): Add purity to guards
    val jad.BipGuard(_) = jad.BipGuard.get(m).get

    if (m.returnType != TBool[Pre]()) { throw WrongGuardReturnType(m) }

    val guard = new BipGuard[Post](
      rw.collectInScope(rw.bipIncomingDataScopes) { m.parameters.foreach(rewriteParameter) },
      rw.dispatch(m.body.get),
      tt, true
    )(m.blame)(SourceNameOrigin(m.name, m.o))
    guard.succeedDefault(m)
  }

  def rewriteOutgoingData(m: JavaMethod[Pre]): Unit = {
    val jad.BipData(name) = jad.BipData.get(m).get

    new BipOutgoingData(
      getBipData(name, m.returnType)(m.o),
      rw.dispatch(m.body.get),
      jad.BipPure.isPure(m)
    )(m.blame)(SourceNameOrigin(m.name, m.o)).succeedDefault(m)
  }

  def generateComponent(cls: JavaClass[Pre], constructors: Seq[Ref[Post, Procedure[Post]]]): Unit = {
    val jad.BipComponent(name, initialState) = jad.BipComponent.get(cls).get
    val invariant: Expr[Pre] = jad.BipInvariant.get(cls) match {
      case Some(value) => value.expr
      case None => tt[Pre]
    }

    // Create bip component marker declaration
    new BipComponent(constructors, rw.dispatch(invariant), getJavaBipStatePredicate(initialState)
      )(cls.o).declareDefault(rw)

    // Create bip state predicate declarations
    cls.modifiers.collect {
      case ja: JavaAnnotation[Pre] => (ja, ja.data)
    }.collect {
      case (ja, Some(bsp @ jad.BipStatePredicate(_, _))) => (ja, bsp)
    }.foreach { case (ja, bspData) =>
      val bspNode = new BipStatePredicate[Post](rw.dispatch(bspData.expr))(SourceNameOrigin(bspData.name, ja.o)).declareDefault(rw)
      bipStatePredicates.update(bspData, bspNode)
    }
  }
}

package vct.col.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.lang.LangBipToCol.{InconsistentBipDataType, WrongTransitionReturnType}
import vct.col.origin.{DiagnosticOrigin, Origin, SourceNameOrigin}
import vct.col.ref.Ref
import vct.col.resolve.ctx.{ImplicitDefaultJavaBipStatePredicate, JavaBipStatePredicateTarget, RefJavaBipGuard, RefJavaBipStatePredicate}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.col.resolve.lang.{JavaAnnotationData => jad}
import vct.col.rewrite.lang.LangSpecificToCol
import vct.result.VerificationError.UserError

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

  val defaultStatePredicates: mutable.Map[String, BipStatePredicate[Post]] = mutable.Map()
  val statePredicates: SuccessionMap[jad.BipStatePredicate[Pre], BipStatePredicate[Post]] = SuccessionMap()
  val components: SuccessionMap[String, BipComponent[Post]] = SuccessionMap()
  val datas: SuccessionMap[String, BipData[Post]] = SuccessionMap()
  val ports: SuccessionMap[String, BipPort[Post]] = SuccessionMap()
  var dataTypes: ListMap[String, Type[Pre]] = ListMap()

  val javaParamSucc: SuccessionMap[JavaParam[Pre], BipIncomingData[Post]] = SuccessionMap()
  val javaMethodSuccTransition: SuccessionMap[JavaMethod[Pre], BipTransition[Post]] = SuccessionMap()
  val javaMethodSuccGuard: SuccessionMap[JavaMethod[Pre], BipGuard[Post]] = SuccessionMap()
  val javaMethodSuccOutgoingData: SuccessionMap[JavaMethod[Pre], BipOutgoingData[Post]] = SuccessionMap()

  def getJavaBipStatePredicate(t: JavaBipStatePredicateTarget[Pre]): Ref[Post, BipStatePredicate[Post]] = t match {
    case RefJavaBipStatePredicate(decl) => statePredicates.ref(decl.data.get.asInstanceOf[jad.BipStatePredicate[Pre]])
    case ImplicitDefaultJavaBipStatePredicate(state) => defaultStatePredicates.getOrElseUpdate(state, {
      rw.classDeclarations.declare(new BipStatePredicate[Post](tt)(DiagnosticOrigin))
    }).ref
  }

  def getBipData(name: String, expectedType: Type[Pre])(implicit o: Origin): Ref[Post, BipData[Post]] = {
    dataTypes.get(name) match {
      case Some(preType) =>
        // No subtyping (yet): references of a data must all have exact same type
        if (preType != expectedType) {
          throw InconsistentBipDataType(datas(name), o, expectedType)
        } else {
          datas(name).ref
        }
      case None =>
        dataTypes = dataTypes.updated(name, expectedType)
        datas(name) = rw.classDeclarations.declare(new BipData[Post](rw.dispatch(expectedType)))
        datas(name).ref
    }
  }

  def rewriteParameter(p: JavaParam[Pre]): Unit = {
    val jad.BipData(name) = jad.BipData.get(p).get
    javaParamSucc(p) =
      rw.bipIncomingDatas.declare(new BipIncomingData(getBipData(name, p.t)(p.o))(p.o))
  }

  def rewriteTransition(m: JavaMethod[Pre]): Unit = {
    val jad.BipTransition(portName, source, target, guard, requires, ensures) = jad.BipTransition.get(m).get

    if (m.returnType != TVoid[Pre]()) { throw WrongTransitionReturnType(m) }

    val trans = rw.bipIncomingDatas.scope {
      new BipTransition[Post](
        ports.ref(portName),
        getJavaBipStatePredicate(source),
        getJavaBipStatePredicate(target),
        rw.bipIncomingDatas.collect {
          m.parameters.map(rewriteParameter)
        }._1,
        guard.map(javaMethodSuccGuard.ref(_)),
        rw.dispatch(requires),
        rw.dispatch(ensures),
        rw.dispatch(m.body.get))(m.blame)(SourceNameOrigin(m.name, m.o))
    }

    javaMethodSuccTransition(m) = rw.classDeclarations.declare(trans)
  }

  def local(local: JavaLocal[Pre]): Expr[Post] = {
    val RefJavaBipGuard(method) = local.ref
    // FunctionInvocation!
    ???
  }

  def local(local: JavaLocal[Pre], decl: JavaParam[Pre]): Expr[Post] = {
    val jad.BipData(_) = jad.BipData.get(decl).get
    BipLocalIncomingData(javaParamSucc.ref[Post, BipIncomingData[Post]](decl))(local.o)
  }

  def rewriteGuard(m: JavaMethod[Pre]): Unit = {
    // TODO (RR): Add ensures to @Guard
    // TODO (RR): Add purity to guards
    val jad.BipGuard(_) = jad.BipGuard.get(m).get

    if (m.returnType != TBool[Pre]()) { throw LangBipToCol.WrongGuardReturnType(m) }

    rw.bipIncomingDatas.scope {
      javaMethodSuccGuard(m) = rw.classDeclarations.declare(new BipGuard[Post](
        rw.bipIncomingDatas.collect {
          m.parameters.foreach(rewriteParameter)
        }._1,
        rw.dispatch(m.body.get),
        tt, true
      )(m.blame)(SourceNameOrigin(m.name, m.o)))
    }
  }

  def rewriteOutgoingData(m: JavaMethod[Pre]): Unit = {
    val jad.BipData(name) = jad.BipData.get(m).get

    javaMethodSuccOutgoingData(m) = rw.classDeclarations.declare(
      new BipOutgoingData(
        getBipData(name, m.returnType)(m.o),
        rw.dispatch(m.body.get),
        jad.BipPure.isPure(m)
      )(m.blame)(SourceNameOrigin(m.name, m.o)))
  }

  def generateComponent(cls: JavaClass[Pre], constructors: Seq[Ref[Post, Procedure[Post]]]): Unit = {
    val jad.BipComponent(name, initialState) = jad.BipComponent.get(cls).get
    val allPorts = jad.BipPort.getAll(cls)


    val invariant: Expr[Pre] = jad.BipInvariant.get(cls) match {
      case Some(value) => value.expr
      case None => tt[Pre]
    }

    // Create bip component marker declaration
    components(name) = rw.classDeclarations.declare(
      new BipComponent(constructors, rw.dispatch(invariant), getJavaBipStatePredicate(initialState))(cls.o))

    // Create bip state predicates
    jad.BipStatePredicate.getAll(cls).foreach { case bspData @ jad.BipStatePredicate(name, expr) =>
      val bspNode = rw.classDeclarations.declare(
        new BipStatePredicate[Post](rw.dispatch(expr))(SourceNameOrigin(name, bspData.o)))
      statePredicates(bspData) = bspNode
    }

    // Create bip ports
    allPorts.foreach { port =>
      assert(port.portType == BipEnforceable[Pre]())
      ports(port.name) = rw.classDeclarations.declare(
        new BipPort(rw.dispatch(port.portType))(SourceNameOrigin(port.name, port.o)))
    }
  }

  def generateSynchron(port1: String, port2: String): Unit =
    rw.globalDeclarations.declare(
      new BipSynchron[Post](ports.ref(port1), ports.ref(port2))(DiagnosticOrigin))

  def generateDataBinding(data1: String, data2: String): Unit = {
    rw.globalDeclarations.declare(
      new BipDataBinding[Post](datas.ref(data1), datas.ref(data2))(DiagnosticOrigin))
  }
}

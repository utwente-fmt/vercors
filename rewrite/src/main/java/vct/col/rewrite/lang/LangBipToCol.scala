package vct.col.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.ast.lang.JavaAnnotationEx
import vct.col.lang.LangBipToCol.{BipDataOrigin, BipDataWireOrigin, BipIncomingDataInconsistentType, BipPortOrigin, WrongTransitionReturnType}
import vct.col.origin.{DiagnosticOrigin, Origin, SourceNameOrigin}
import vct.col.print.Printer
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

  case class BipIncomingDataInconsistentType(data: BipData[_], param: JavaParam[_]) extends UserError {
    override def code: String = "bipInconsistentDataType"
    override def text: String = Origin.messagesInContext(Seq(
      (data.o, s"The data defined here..."),
      (param.o, s"... is expected to have type ${param.t} by the usage here")
    ))
  }

  case class BipPortOrigin(ns: JavaNamespace[_], cls: JavaClass[_], port: jad.BipPort[_]) extends Origin {
    override def preferredName: String = {
      val fqn = (ns.name.map(Seq(_)).getOrElse(Seq()) :+ cls.name).mkString(".")
      s"($fqn,${port.name})"
    }

    override def context: String = port.o.context
    override def inlineContext: String = port.o.inlineContext
    override def shortPosition: String = port.o.shortPosition
  }

  case class BipDataWireOrigin(cls0: JavaClass[_], port0: String, cls1: JavaClass[_], port1: String, o: Origin) extends Origin {
    override def preferredName: String = s"(${cls0.name},$port0)->(${cls1.name},$port1)"
    override def context: String = o.context
    override def inlineContext: String = o.inlineContext
    override def shortPosition: String = o.shortPosition
  }

  case class BipDataOrigin(ns: JavaNamespace[_], cls: JavaClass[_], data: jad.BipData[_]) extends Origin {
    override def preferredName: String = {
      val fqn = (ns.name.map(Seq(_)).getOrElse(Seq()) :+ cls.name).mkString(".")
      s"($fqn,${data.name})"
    }

    override def context: String = data.o.context
    override def inlineContext: String = data.o.inlineContext
    override def shortPosition: String = data.o.shortPosition
  }
}

// TODO (RR): More specific origins (e.g. avoid SourceNameOrigin), errors, panicBlame...

case class LangBipToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val defaultStatePredicates: mutable.Map[String, BipStatePredicate[Post]] = mutable.Map()
  val statePredicates: SuccessionMap[jad.BipStatePredicate[Pre], BipStatePredicate[Post]] = SuccessionMap()
  val components: SuccessionMap[String, BipComponent[Post]] = SuccessionMap()
  val dataOut: SuccessionMap[(JavaClass[Pre], String), BipOutgoingData[Post]] = SuccessionMap()
  val dataIn: SuccessionMap[(JavaClass[Pre], String), BipIncomingData[Post]] = SuccessionMap()
  val dataInPreType: mutable.LinkedHashMap[(JavaClass[Pre], String), Type[Pre]] = new mutable.LinkedHashMap()
  val ports: SuccessionMap[(JavaClass[Pre], String), BipPort[Post]] = SuccessionMap()

  val glueSucc: SuccessionMap[JavaBipGlueContainer[Pre], BipGlue[Post]] = SuccessionMap()

  val javaParamSucc: SuccessionMap[JavaParam[Pre], BipIncomingData[Post]] = SuccessionMap()
  val javaMethodSuccTransition: SuccessionMap[(JavaMethod[Pre], jad.BipTransition[Pre]), BipTransition[Post]] = SuccessionMap()
  val javaMethodSuccGuard: SuccessionMap[JavaMethod[Pre], BipGuard[Post]] = SuccessionMap()
  val javaMethodSuccOutgoingData: SuccessionMap[JavaMethod[Pre], BipOutgoingData[Post]] = SuccessionMap()

  def currentClass(): JavaClass[Pre] = rw.java.currentJavaClass.top.asInstanceOf[JavaClass[Pre]]

  def getJavaBipStatePredicate(t: JavaBipStatePredicateTarget[Pre]): Ref[Post, BipStatePredicate[Post]] = t match {
    case RefJavaBipStatePredicate(_, decl) => statePredicates.ref(decl.data.get.asInstanceOf[jad.BipStatePredicate[Pre]])
    case ImplicitDefaultJavaBipStatePredicate(state) => defaultStatePredicates.getOrElseUpdate(state, {
      rw.classDeclarations.declare(new BipStatePredicate[Post](tt)(DiagnosticOrigin))
    }).ref
  }

  def rewriteParameter(p: JavaParam[Pre]): Ref[Post, BipIncomingData[Post]] = {
    val annData @ jad.BipData(name) = jad.BipData.get(p).get
    val dataTuple = (currentClass(), name)
    val data = dataInPreType.get(dataTuple) match {
      case Some(preType) =>
        if (preType == p.t) {
          dataIn(dataTuple)
        } else {
          throw BipIncomingDataInconsistentType(dataIn(dataTuple), p)
        }
      case None =>
        dataIn(dataTuple) = rw.classDeclarations.declare(new BipIncomingData(rw.dispatch(p.t))(
          BipDataOrigin(rw.java.namespace.top, currentClass(), annData)))
        dataInPreType(dataTuple) = p.t
        dataIn(dataTuple)
    }
    javaParamSucc(p) = data
    data.ref
  }

  def rewriteTransition(m: JavaMethod[Pre]): Unit = jad.BipTransition.get(m).foreach(rewriteTransition(m, _))

  def rewriteTransition(m: JavaMethod[Pre], transition: jad.BipTransition[Pre]): Unit = {
    val jad.BipTransition(portName, source, target, guardText, guard, requires, ensures) = transition

    if (m.returnType != TVoid[Pre]()) { throw WrongTransitionReturnType(m) }

    val signature = BipTransitionSignature[Post](
      portName,
      source.name,
      target.name,
      guardText
    )(transition.o)

    val trans = new BipTransition[Post](
      signature,
      ports.ref((currentClass(), portName)),
      getJavaBipStatePredicate(source),
      getJavaBipStatePredicate(target),
      m.parameters.map(rewriteParameter),
      guard.map(rw.dispatch).getOrElse(tt),
      rw.dispatch(requires),
      rw.dispatch(ensures),
      rw.dispatch(m.body.get))(m.blame)(SourceNameOrigin(m.name, m.o))

    javaMethodSuccTransition((m, transition)) = rw.classDeclarations.declare(trans)
  }

  def local(local: JavaLocal[Pre]): Expr[Post] = {
    val Some(RefJavaBipGuard(method)) = local.ref
    BipGuardInvocation(
      ThisObject(rw.java.javaInstanceClassSuccessor.ref[Post, Class[Post]](rw.java.currentJavaClass.top))(local.o),
      javaMethodSuccGuard.ref[Post, BipGuard[Post]](method)
    )(local.o)
  }

  def local(local: JavaLocal[Pre], decl: JavaParam[Pre]): Expr[Post] = {
    val data @ jad.BipData(_) = jad.BipData.get(decl).get
    BipLocalIncomingData(javaParamSucc.ref[Post, BipIncomingData[Post]](decl))(local.o)
  }

  def rewriteGuard(m: JavaMethod[Pre]): Unit = {
    // TODO (RR): Add ensures to @Guard
    // TODO (RR): Add purity to guards
    val jad.BipGuard(_) = jad.BipGuard.get(m).get

    if (m.returnType != TBool[Pre]()) { throw LangBipToCol.WrongGuardReturnType(m) }

    javaMethodSuccGuard(m) = rw.classDeclarations.declare(new BipGuard[Post](
      m.parameters.map(rewriteParameter),
      rw.dispatch(m.body.get),
      true
    )(m.blame)(SourceNameOrigin(m.name, m.o)))
  }

  def rewriteOutgoingData(m: JavaMethod[Pre]): Unit = {
    val data @ jad.BipData(name) = jad.BipData.get(m).get
    assert(jad.BipPure.isPure(m), m.o.messageInContext("The following outgoing data should be pure"))

    javaMethodSuccOutgoingData(m) = rw.classDeclarations.declare(
      new BipOutgoingData(
        rw.dispatch(m.returnType),
        rw.dispatch(m.body.get),
        jad.BipPure.isPure(m)
      )(m.blame)(BipDataOrigin(rw.java.namespace.top, currentClass(), data)))
    dataOut((currentClass(), name)) = javaMethodSuccOutgoingData(m)
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
      new BipComponent(
        rw.java.namespace.top.pkg.get.names :+ cls.name,
        constructors,
        rw.dispatch(invariant),
        getJavaBipStatePredicate(initialState))(cls.o))

    // Create bip state predicates
    jad.BipStatePredicate.getAll(cls).foreach { case bspData @ jad.BipStatePredicate(name, expr) =>
      val bspNode = rw.classDeclarations.declare(
        new BipStatePredicate[Post](rw.dispatch(expr))(SourceNameOrigin(name, bspData.o)))
      statePredicates(bspData) = bspNode
    }

    // Create bip ports
    allPorts.foreach { port =>
      assert(port.portType == BipEnforceable[Pre]())
      ports((cls, port.name)) = rw.classDeclarations.declare(
        new BipPort(rw.dispatch(port.portType))(BipPortOrigin(rw.java.namespace.top, currentClass(), port)))
    }
  }

  def rewritePortName(portName: JavaBipGlueName[Pre]): Ref[Post, BipPort[Post]] = {
    val Some((cls, name)) = portName.data
    ports.ref((cls, name))
  }

  def rewriteAccepts(glue: JavaBipGlueAccepts[Pre]): BipGlueAccepts[Post] =
    BipGlueAccepts(rewritePortName(glue.port), glue.others.map(rewritePortName))(glue.o)

  def rewriteRequires(glue: JavaBipGlueRequires[Pre]): BipGlueRequires[Post] =
    BipGlueRequires(rewritePortName(glue.port), glue.others.map(rewritePortName))(glue.o)

  def rewriteSynchron(synchron: JavaBipGlueSynchron[Pre]): (Seq[BipGlueRequires[Post]], Seq[BipGlueAccepts[Post]]) = {
    val p0 = rewritePortName(synchron.port0)
    val p1 = rewritePortName(synchron.port1)
    implicit val o = synchron.o
    (Seq(BipGlueRequires(p0, Seq(p1)), BipGlueRequires(p1, Seq(p0))),
      Seq(BipGlueAccepts(p0, Seq(p1)), BipGlueAccepts(p1, Seq(p0))))
  }

  def rewriteDataWire(wire: JavaBipGlueDataWire[Pre]): BipGlueDataWire[Post] = {
    val Some((clsOut, nameOut)) = wire.dataOut.data
    val Some((clsIn, nameIn)) = wire.dataIn.data
    BipGlueDataWire[Post](
      dataOut.ref((clsOut, nameOut)),
      dataIn.ref((clsIn, nameIn))
    )(BipDataWireOrigin(clsOut, nameOut, clsIn, nameIn, wire.o))
  }

  def rewriteGlue(container: JavaBipGlueContainer[Pre]): Unit = {
    val glue = container.job.asInstanceOf[JavaBipGlue[Pre]]
    val requires = glue.collect { case r: JavaBipGlueRequires[Pre] => r }
    val accepts = glue.collect { case a: JavaBipGlueAccepts[Pre] => a }

    val synchrons = glue.collect { case s: JavaBipGlueSynchron[Pre] => s }
    val pairs = synchrons.map(rewriteSynchron)

    val wires = glue.collect { case w: JavaBipGlueDataWire[Pre] => w }

    glueSucc(container) = rw.globalDeclarations.declare(new BipGlue[Post](
        requires.map(rewriteRequires) ++ pairs.flatMap(_._1),
        accepts.map(rewriteAccepts) ++ pairs.flatMap(_._2),
        wires.map(rewriteDataWire)
        )(glue.blame)(glue.o))
  }
}

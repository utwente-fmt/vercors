package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers.{contract, _}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

import scala.::
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case object InstantiateBipSynchronizations extends RewriterBuilder {
  override def key: String = "instantiateBipSynchronizations"
  override def desc: String = "Convert synchronizations of ports combinatorially to synchronizations of transitions"
}

case class InstantiateBipSynchronizations[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  var program: Program[Pre] = null
  lazy val ports: IndexedSeq[BipPort[Pre]] = program.transSubnodes.collect { case p: BipPort[Pre] => p }.toIndexedSeq
  lazy val portToTransitions: ListMap[BipPort[Pre], Seq[BipTransition[Pre]]] = {
    ListMap.from(ports.map { port =>
      (port, program.transSubnodes.collect { case t: BipTransition[Pre] if t.port.decl == port => t }.toIndexedSeq)
    })
  }
  lazy val portSynchrons = program.transSubnodes.collect { case s: BipPortSynchronization[Pre] => s }.toIndexedSeq

  val transitionSynchrons = new ArrayBuffer[BipTransitionSynchronization[Post]]()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program
    val p = super.dispatch(program)
    if (transitionSynchrons.nonEmpty) {
      logger.info(s"Converted ${portSynchrons.size} port synchronizations into ${transitionSynchrons.size} transition synchronizations")
      transitionSynchrons.foreach(t => logger.debug(t.summarize))
    }
    p
  }

  // Given lists [a b c] [d e f] [g h], I want: [a d g], [a d h], [a e g], [a e h], etc.
  def pickOneFromEach[T](elems: Seq[Seq[T]]): Seq[Seq[T]] = elems match {
    case Seq(elems) => elems.map(Seq(_))
    case Seq() => Seq()
    case elem +: elems =>
      val tails = pickOneFromEach(elems)
      elem.flatMap(e => tails.map(t => e +: t))
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case sync: BipPortSynchronization[Pre] =>
      val wires = sync.wires.map(dispatch)
      val transitionSets = pickOneFromEach(sync.ports.map { case Ref(port) => portToTransitions(port) })
      val newSyncs = transitionSets.map { transitions =>
        globalDeclarations.declare(BipTransitionSynchronization[Post](transitions.map(t => succ[BipTransition[Post]](t)), wires))
      }
      transitionSynchrons.addAll(newSyncs)
      sync.drop()

    case _ => rewriteDefault(decl)
  }
}

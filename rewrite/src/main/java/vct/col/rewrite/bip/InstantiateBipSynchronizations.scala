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

case object InstantiateBipSynchronizations extends RewriterBuilder {
  override def key: String = "instantiateBipSynchronizations"
  override def desc: String = "Convert synchronizations of ports combinatorially to synchronizations of transitions"
}

case class InstantiateBipSynchronizations[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  import vct.col.rewrite.bip.InstantiateBipSynchronizations._

  var program: Program[Pre] = null
  lazy val ports: Seq[BipPort[Pre]] = program.transSubnodes.collect { case p: BipPort[Pre] => p }
  lazy val portToTransitions: ListMap[BipPort[Pre], Seq[BipTransition[Pre]]] = {
    ListMap.from(ports.map { port =>
      (port, program.transSubnodes.collect { case t: BipTransition[Pre] if t.port.decl == port => t })
    })
  }

  var convertedSynchronizations = 0
  var resultingSynchronizations = 0

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program
    val p = super.dispatch(program)
    if (convertedSynchronizations > 0) {
      logger.info(s"Converted $convertedSynchronizations port synchronizations into $resultingSynchronizations transition synchronizations")
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
      convertedSynchronizations += 1
      resultingSynchronizations += transitionSets.size
      transitionSets.map { transitions =>
        globalDeclarations.declare(BipTransitionSynchronization[Post](transitions.map(succ[BipTransition[Post]](_)), wires))
      }
      sync.drop()

    case _ => rewriteDefault(decl)
  }
}

package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{ApplicableContract, Choreography, Declaration, Endpoint, EndpointUse, Expr, IterationContract, LoopContract, LoopInvariant, Program, SplitAccountedPredicate, UnitAccountedPredicate}
import vct.col.origin.{AssertFailed, Blame, BranchUnanimityFailed, LoopUnanimityNotEstablished, LoopUnanimityNotMaintained, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.EncodeChorBranchUnanimity.{ForwardBranchUnanimity, ForwardLoopUnanimityNotEstablished, ForwardLoopUnanimityNotMaintained}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError

import scala.collection.mutable


object EncodeEndpointInequalities extends RewriterBuilder {
  override def key: String = "encodeEndpointInequalities"
  override def desc: String = "Encodes inequalities of endpoints in contracts and loop invariants within choreographies"
}

case class EncodeEndpointInequalities[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre]  {

  val inequalityMap = mutable.LinkedHashMap[Choreography[Pre], Expr[Post]]()
  def currentInequality: Expr[Post] = inequalityMap.getOrElseUpdate(currentChoreography.top, makeInequalities(currentChoreography.top))
  def makeInequalities(chor: Choreography[Pre]): Expr[Post] = {
    // Computation of the inequality expression is done lazily such that when it is computed within a choreography,
    // the endpoints are in scope and their successors will be available to `succ`.
    def makeInequalitySets(endpoints: Seq[Endpoint[Pre]]): Seq[(Endpoint[Pre], Seq[Endpoint[Pre]])] = endpoints match {
      case xs if xs.length <= 1 => Seq()
      case endpoint +: targets =>
        (endpoint, targets) +: makeInequalitySets(targets)
    }
    implicit val o = chor.o
    foldStar(makeInequalitySets(chor.endpoints).flatMap { case (endpoint, others) =>
      others.map { other =>
        EndpointUse[Post](succ(endpoint)) !== EndpointUse[Post](succ(other))
      }
    })
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case chor: Choreography[Pre] => currentChoreography.having(chor) {
      globalDeclarations.succeed(chor, chor.rewrite(contract = chor.contract.rewriteDefault()))
    }
    case _ => super.dispatch(decl)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = contract match {
    case InChor(_, contract) =>
      implicit val o = contract.o
      contract.rewrite(
        requires = SplitAccountedPredicate(UnitAccountedPredicate(currentInequality), dispatch(contract.requires)),
        ensures = SplitAccountedPredicate(UnitAccountedPredicate(currentInequality), dispatch(contract.ensures)))
    case _ => super.dispatch(contract)
  }
  
  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] = contract match {
    case InChor(_, inv: LoopInvariant[Pre]) =>
      implicit val o = contract.o
      inv.rewrite(currentInequality &* dispatch(inv.invariant))
    case InChor(_, contract: IterationContract[Pre]) =>
      implicit val o = contract.o
      contract.rewrite(
        requires = currentInequality &* dispatch(contract.requires),
        ensures = currentInequality &* dispatch(contract.ensures))
    case _ => super.dispatch(contract)
  }
}

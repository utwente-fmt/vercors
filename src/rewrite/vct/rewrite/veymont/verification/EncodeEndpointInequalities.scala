package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.veymont.VeymontContext

import scala.collection.mutable

object EncodeEndpointInequalities extends RewriterBuilder {
  override def key: String = "encodeEndpointInequalities"
  override def desc: String =
    "Encodes inequalities of endpoints in contracts and loop invariants within choreographies, as well as required inequalities on the sender and receiver of communicate statements"
}

case class EncodeEndpointInequalities[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  val inequalityMap = mutable.LinkedHashMap[Choreography[Pre], Expr[Post]]()
  def currentInequality: Expr[Post] =
    inequalityMap.getOrElseUpdate(
      currentChoreography.top,
      makeInequalities(currentChoreography.top),
    )
  def makeInequalities(chor: Choreography[Pre]): Expr[Post] = {
    // Computation of the inequality expression is done lazily such that when it is computed within a choreography,
    // the endpoints are in scope and their successors will be available to `succ`.
    def makeInequalitySets(
        endpoints: Seq[Endpoint[Pre]]
    ): Seq[(Endpoint[Pre], Seq[Endpoint[Pre]])] =
      endpoints match {
        case xs if xs.length <= 1 => Seq()
        case endpoint +: targets =>
          (endpoint, targets) +: makeInequalitySets(targets)
      }
    implicit val o = chor.o
    foldStar(
      makeInequalitySets(chor.endpoints).flatMap { case (endpoint, others) =>
        others.map { other =>
          EndpointName[Post](succ(endpoint)) !== EndpointName[Post](succ(other))
        }
      } ++ chor.endpoints.map { endpoint =>
        EndpointName[Post](succ(endpoint)) !== Null()
      }
    )
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          chor.rewrite(
            contract = chor.contract.rewriteDefault(),
            preRun = {
              val preRun = chor.preRun.map(dispatch)
                .getOrElse(Block(Seq())(chor.o))
              implicit val o = chor.o
              val endpointPairs =
                if (chor.endpoints.nonEmpty)
                  chor.endpoints.zip(chor.endpoints.tail)
                else
                  Seq()
              Some(Block(Seq(
                preRun,
                Assume[Post](foldAnd(endpointPairs.map { case (alice, bob) =>
                  Neq[Post](EndpointName(succ(alice)), EndpointName(succ(bob)))
                })),
              )))
            },
          ).succeed(chor)
        }
      case _ => super.dispatch(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] =
    contract match {
      case InChor(_, contract) =>
        implicit val o = contract.o
        contract.rewrite(
          requires = SplitAccountedPredicate(
            UnitAccountedPredicate(currentInequality),
            dispatch(contract.requires),
          ),
          ensures = SplitAccountedPredicate(
            UnitAccountedPredicate(currentInequality),
            dispatch(contract.ensures),
          ),
        )
      case _ => super.dispatch(contract)
    }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(_, inv: LoopInvariant[Pre]) =>
        implicit val o = contract.o
        inv.rewrite(currentInequality &* dispatch(inv.invariant))
      case InChor(_, contract: IterationContract[Pre]) =>
        implicit val o = contract.o
        contract.rewrite(
          requires = currentInequality &* dispatch(contract.requires),
          ensures = currentInequality &* dispatch(contract.ensures),
        )
      case _ => super.dispatch(contract)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case comm: CommunicateStatement[Pre] =>
        implicit val o = comm.o
        val sender = comm.inner.sender.get.decl
        val receiver = comm.inner.receiver.get.decl
        if (receiver.t == sender.t)
          Block(Seq(
            Assert(
              EndpointName[Post](succ(receiver)) !==
                EndpointName[Post](succ(sender))
            )(AssertFailedToParticipantsNotDistinct(comm.inner)),
            comm.rewriteDefault(),
          ))
        else
          comm.rewriteDefault()

      case _ => statement.rewriteDefault()
    }
}

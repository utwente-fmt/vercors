package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.PanicBlame
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.AstMatchHelpers.EndpointName
import vct.rewrite.veymont.VeymontContext
import vct.rewrite.veymont.verification.EncodeChoreography.AssertFailedToParticipantsNotDistinct

import scala.collection.mutable

object EncodeEndpointInequalities extends RewriterBuilder {
  override def key: String = "encodeEndpointInequalities"
  override def desc: String =
    "Encodes inequalities of endpoints in contracts and loop invariants within choreographies, as well as required inequalities on the sender and receiver of communicate statements"
}

/*
 * This pass is currently incomplete: for a set of singular endpoints E, and a set of endpoint families F, it only
 * encodes:
 *
 * - forall a, b : E; a != b
 * - forall i, j : {0..<|F|}; F[i] != F[j]
 *
 * This could be made more complete by actually mapping all endpoints, and endpoint families and their indices, to
 * the ints and back. Probably similar to how the array encoding accomplishes this. The only tricky part is
 * incorporating the singular endpoints in this encoding. Not theoretically difficult, just fiddly with the axioms.
 *
 */
case class EncodeEndpointInequalities[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  val inequalityMap = mutable.LinkedHashMap[Choreography[Pre], Expr[Post]]()
  def getInequality(chor: Choreography[Pre]): Expr[Post] =
    inequalityMap
      .getOrElseUpdate(chor, makeInequalities(currentChoreography.top))

  def makeInequalities(chor: Choreography[Pre]): Expr[Post] = {
    implicit val o = chor.o
    val f =
      new ADTFunction[Post](Seq(new Variable(TAnyValue())), TInt())(
        o.where(name = "f")
      )
    val inv =
      new ADTFunction[Post](Seq(new Variable(TInt())), TAnyValue())(
        o.where(name = "f_inv")
      )
    val adt =
      new AxiomaticDataType[Post](Seq(f, inv), Seq())(
        o.where(name = o.getPreferredNameOrElse().camel.take(2) + "_B")
      )
    globalDeclarations.declare(adt)

    def appf(args: Expr[Post]*): Expr[Post] =
      ADTFunctionInvocation[Post](None, f.ref, args)

    def appinv(args: Expr[Post]*): Expr[Post] =
      ADTFunctionInvocation[Post](None, inv.ref, args)

    // TODO (RR): Is the constriant involving appinv even necessary?
    //            Distinctiveness already follows from mapping to integers, right
    // TODO (RR): Probably will want to put the constraints below into a function. It makes the generated code
    //            hard to read.

    val (_, constraints): (Expr[Post], Expr[Post]) =
      chor.endpoints.foldLeft[(Expr[Post], Expr[Post])]((const(0), tt)) {
        case ((baseInt, constraints), endpoint) if endpoint.isSingle =>
          val newBaseInt: Expr[Post] = baseInt + const(1)
          val newEp = CtExpr(CommTargetEndpoint[Post](succ(endpoint)))
          val newConstraints: Expr[Post] =
            constraints |&&| (appf(newEp) === baseInt) |&&|
              appinv(appf(newEp)) === newEp

          (newBaseInt, newConstraints)
        case ((baseInt, constraints), endpoint) if endpoint.isFamily =>
          val fam = EndpointFamilyExpr[Post](succ(endpoint))
          val newBaseInt: Expr[Post] = baseInt + Size(fam)
          val newConstraints: Expr[Post] =
            constraints |&&| forrange[Post](
              Size(fam),
              (i: Local[Post]) => {
                val fam_i =
                  SeqSubscript(fam, i)(PanicBlame(
                    "Should not go out of bounds"
                  ))
                (appf(fam_i) === baseInt + i) |&&| appinv(appf(fam_i)) === fam_i
              },
            )
          (newBaseInt, newConstraints)
      }

    constraints |&&| foldAnd(bounds(chor))
  }

  def bounds(chor: Choreography[Pre]): Seq[Expr[Post]] =
    chor.endpoints.collect {
      case ep if ep.isFamily =>
        implicit val o = ep.o
        Size(EndpointFamilyExpr[Post](succ(ep))) ===
          (dispatch(ep.range.get.high) - dispatch(ep.range.get.low))
    }

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    super.dispatch(p)
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
              Some(Block(Seq(preRun, Assume[Post](getInequality(chor)))))
            },
          ).succeed(chor)
        }
      case _ => super.dispatch(decl)
    }

  override def dispatch(
      contract: ApplicableContract[Pre]
  ): ApplicableContract[Post] =
    contract match {
      case InChor(chor, contract) =>
        implicit val o = contract.o
        contract.rewrite(
          requires = SplitAccountedPredicate(
            UnitAccountedPredicate(getInequality(chor)),
            dispatch(contract.requires),
          ),
          ensures = SplitAccountedPredicate(
            UnitAccountedPredicate(getInequality(chor)),
            dispatch(contract.ensures),
          ),
        )
      case _ => super.dispatch(contract)
    }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(chor, inv: LoopInvariant[Pre]) =>
        implicit val o = contract.o
        inv.rewrite(getInequality(chor) &* dispatch(inv.invariant))
      case InChor(chor, contract: IterationContract[Pre]) =>
        implicit val o = contract.o
        contract.rewrite(
          requires = getInequality(chor) &* dispatch(contract.requires),
          ensures = getInequality(chor) &* dispatch(contract.ensures),
        )
      case _ => super.dispatch(contract)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case comm: CommunicateStatement[Pre]
          if comm.inner.sender.get.isSingle &&
            comm.inner.receiver.get.isSingle =>
        // TODO (RR): I guess for endpoint families we should implement an overlapping check here
        implicit val o = comm.o
        val sender = comm.inner.sender.get
        val receiver = comm.inner.receiver.get
        if (receiver.ref.decl.singleType == sender.ref.decl.singleType)
          Block(Seq(
            Assert(CtExpr(dispatch(receiver)) !== CtExpr(dispatch(sender)))(
              AssertFailedToParticipantsNotDistinct(comm.inner)
            ),
            comm.rewriteDefault(),
          ))
        else
          comm.rewriteDefault()

      case _ => statement.rewriteDefault()
    }
}

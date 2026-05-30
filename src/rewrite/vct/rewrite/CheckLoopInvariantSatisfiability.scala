package vct.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

case object CheckLoopInvariantSatisfiability extends RewriterBuilder {
  override def key: String = "checkInvSat"
  override def desc: String =
    "Check that loop invariants are not internally contradictory (i.e. unsatisfiable)."
}

case class CheckLoopInvariantSatisfiability[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case loop @ Loop(_, _, _, li @ LoopInvariant(inv, _), _) =>
        implicit val o: Origin = loop.o.where(prefix = "checkInvSat")
        val refuteBlame = new Blame[RefuteFailed] {
          override def blame(error: RefuteFailed): Unit =
            li.blame.blame(LoopInvariantUnsatisfiable(li))
        }
        Block(Seq(
          Extract(
            FramedProof(
              pre = tt,
              body = Block(Seq[Statement[Post]](
                Inhale(dispatch(inv)),
                Refute(ff)(refuteBlame),
              )),
              post = tt,
            )(TrueSatisfiable),
            decreases = None,
          )(PanicBlame("loop invariant satisfiability check has no termination measure")),
          loop.rewriteDefault(),
        ))

      case other => other.rewriteDefault()
    }
}

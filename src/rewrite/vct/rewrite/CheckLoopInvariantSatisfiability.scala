package vct.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.rewrite.util.WellDefinednessConditions
import vct.col.util.AstBuildHelpers._

// Checks that loop invariants are satisfiable by inhaling the invariant in an isolated
// FramedProof and then refuting false. If refute fires, the invariant is unsatisfiable.
// WD conditions are inhaled in separate statements before the invariant so each one
// is in the prover state when the next expression's WD is checked.
case object CheckLoopInvariantSatisfiability extends RewriterBuilderArg[Boolean] {
  override def key: String = "checkInvSat"
  override def desc: String =
    "Check that loop invariants are not internally contradictory (i.e. unsatisfiable)."

  case class LoopInvariantUnsatisfiableBlame(li: LoopInvariant[_])
      extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      li.blame.blame(LoopInvariantUnsatisfiable(li))
  }
}

case class CheckLoopInvariantSatisfiability[Pre <: Generation](doCheck: Boolean = true)
    extends Rewriter[Pre] {
  import CheckLoopInvariantSatisfiability._

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    if (!doCheck) stat.rewriteDefault()
    else stat match {
      case loop @ Loop(_, _, _, li @ LoopInvariant(inv, _), _) =>
        implicit val o: Origin = li.o.where(prefix = "checkInvSat")
        Block[Post](Seq[Statement[Post]](
          Extract(
            FramedProof(
              tt,
              {
                val wdConditions = WellDefinednessConditions.collect(inv)
                Block[Post](
                  wdConditions.map(wdc => Inhale(dispatch(wdc))) :+
                  Inhale(dispatch(inv)) :+
                  Refute(ff)(LoopInvariantUnsatisfiableBlame(li))
                )
              },
              tt,
            )(PanicBlame("FramedProof with trivial pre/post cannot fail")),
            None,
          )(PanicBlame("Extract without termination measure cannot fail")),
          loop.rewriteDefault(),
        ))
      case other => other.rewriteDefault()
    }
}

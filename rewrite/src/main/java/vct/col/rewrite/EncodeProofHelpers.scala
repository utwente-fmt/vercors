package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.EncodeProofHelpers.{FramedProofLoopInvariantFailed, Indet, Once}
import vct.col.origin.{Blame, ExhaleFailed, FramedProofPostFailed, FramedProofPreFailed, LoopInvariantFailure, LoopInvariantNotEstablished, LoopInvariantNotMaintained, Origin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._

case object EncodeProofHelpers extends RewriterBuilder {
  override def key: String = "proofHelpers"
  override def desc: String = "Encode statements framed with FramedProof, and indeterminate integers."

  case object Once extends Origin {
    override def preferredName: String = "once"
    override def context: String = "[At node generated to execute a while loop once]"
    override def inlineContext: String = "Node generated to execute a while loop once"
    override def shortPosition: String = "generated"
  }

  case object Indet extends Origin {
    override def preferredName: String = "indet"
    override def context: String = "[At node generated to contain an indeterminate integer]"
    override def inlineContext: String = "Node generated to contain an indeterminate integer"
    override def shortPosition: String = "generated"
  }

  case class FramedProofLoopInvariantFailed(proof: FramedProof[_]) extends Blame[LoopInvariantFailure] {
    override def blame(error: LoopInvariantFailure): Unit = error match {
      case LoopInvariantNotEstablished(failure, _) =>
        proof.blame.blame(FramedProofPreFailed(failure, proof))
      case LoopInvariantNotMaintained(failure, _) =>
        proof.blame.blame(FramedProofPostFailed(failure, proof))
    }
  }
}

case class EncodeProofHelpers[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case proof @ FramedProof(pre, body, post) =>
      implicit val o: Origin = stat.o
      val once = new Variable[Post](TBool())(Once)
      Scope(Seq(once), Loop(
        init = assignLocal(once.get, tt),
        cond = once.get,
        update = assignLocal(once.get, ff),

        contract = LoopInvariant(
          (once.get ==> dispatch(pre)) &*
            (!once.get ==> dispatch(post)),
          Some(DecreasesClauseNoRecursion[Post]()),
        )(FramedProofLoopInvariantFailed(proof)),
        body = dispatch(body),
      ))

    case IndetBranch(branches) =>
      // PB: note that if branches == Nil, this is the same as `inhale false`. This is intended.
      implicit val o: Origin = stat.o
      val v = new Variable[Post](TInt())(Indet)
      Scope(Seq(v), Block(Seq(
        Inhale(v.get >= const(0)),
        Inhale(v.get < const(branches.size)),
        branches match {
          case Nil => Block(Nil)
          case branches => Branch(branches.zipWithIndex.map {
            case (arm, no) => (v.get === const(no), dispatch(arm))
          })
        }
      )))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case IndeterminateInteger(min, max) =>
      // PB: note that if max <= min, this is the same as `inhale false`. This is intended.
      implicit val o: Origin = e.o
      val v = new Variable[Post](TInt())(Indet)
      ScopedExpr(Seq(v), With(Block(Seq(
        Inhale(v.get >= dispatch(min)),
        Inhale(v.get < dispatch(max)),
      )), v.get))
    case other => rewriteDefault(other)
  }
}

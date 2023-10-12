package vct.col.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._

case object EncodeProofHelpers extends RewriterBuilderArg[Boolean] {
  override def key: String = "proofHelpers"
  override def desc: String = "Encode statements framed with FramedProof, and indeterminate integers."

  private def Once: Origin = Origin(
    Seq(
      PreferredName("once"),
      Context("[At node generated to execute a while loop once]"),
      InlineContext("Node generated to execute a while loop once"),
      ShortPosition("generated"),
    )
  )

  private def Indet: Origin = Origin(
    Seq(
      PreferredName("indet"),
      Context("[At node generated to contain an indeterminate integer]"),
      InlineContext("Node generated to contain an indeterminate integer"),
      ShortPosition("generated"),
    )
  )

  private def Before: Origin = Origin(
    Seq(
      PreferredName("beforeFrame"),
      Context("[At node generated to indicate the point before a proof frame]"),
      InlineContext("Node generated to indicate the point before a proof frame"),
      ShortPosition("generated"),
    )
  )

  private def BeforeVar(preferredName: String): Origin = Origin(
    Seq(
      PreferredName(preferredName),
      Context("[At variable generated for forperm]"),
      InlineContext("Variable generated for forperm"),
      ShortPosition("generated"),
    )
  )

  case class FramedProofLoopInvariantFailed(proof: FramedProof[_]) extends Blame[LoopInvariantFailure] {
    override def blame(error: LoopInvariantFailure): Unit = error match {
      case LoopInvariantNotEstablished(failure, _) =>
        proof.blame.blame(FramedProofPreFailed(failure, proof))
      case LoopInvariantNotMaintained(failure, _) =>
        proof.blame.blame(FramedProofPostFailed(failure, proof))
      case LoopTerminationMeasureFailed(_) => PanicBlame("There is no termination measure here").blame(error)
    }
  }
}

case class EncodeProofHelpers[Pre <: Generation](inferHeapContextIntoFrame: Boolean = true) extends Rewriter[Pre] {
  import vct.col.rewrite.EncodeProofHelpers._

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case proof @ FramedProof(pre, body, post) =>
      implicit val o: Origin = stat.o

      val beforeLabel = new LabelDecl[Post]()(Before)
      val locValue = new Variable[Post](TAny())(BeforeVar("x"))
      val allLocationsSame =
        ForPermWithValue(locValue, locValue.get === Old(locValue.get, Some(beforeLabel.ref))(PanicBlame("loop body reached after label before it")))
      val allLocationsSameOnInhale =
        if(inferHeapContextIntoFrame) PolarityDependent(allLocationsSame, tt) else tt[Post]

      val once = new Variable[Post](TBool())(Once)
      val loop = Loop(
        init = assignLocal(once.get, tt),
        cond = once.get,
        update = assignLocal(once.get, ff),

        contract = LoopInvariant(
          (once.get ==> (dispatch(pre) &* allLocationsSameOnInhale)) &*
            (!once.get ==> dispatch(post)),
          Some(DecreasesClauseNoRecursion[Post]()),
        )(FramedProofLoopInvariantFailed(proof)),
        body = dispatch(body),
      )

      Scope(Seq(once), Block(Seq(Label(beforeLabel, Block(Nil)), loop)))

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

package vct.rewrite.veymont

import vct.col.ast._
import vct.col.origin.{AssertFailed, Blame, BranchUnanimityFailed, LoopUnanimityNotEstablished, LoopUnanimityNotMaintained, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.EncodeSeqBranchUnanimity.{ForwardBranchUnanimity, ForwardLoopUnanimityNotEstablished, ForwardLoopUnanimityNotMaintained}

object EncodeSeqBranchUnanimity  extends RewriterBuilder {
  override def key: String = "encodeSeqBranchUnanimity"
  override def desc: String = "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in seq_program nodes."

  case class ForwardBranchUnanimity(branch: SeqBranch[_], c1: SeqGuard[_], c2: SeqGuard[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      branch.blame.blame(BranchUnanimityFailed(c1, c2))
  }

  case class ForwardLoopUnanimityNotEstablished(loop: SeqLoop[_], c1: SeqGuard[_], c2: SeqGuard[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      loop.blame.blame(LoopUnanimityNotEstablished(c1, c2))
  }

  case class ForwardLoopUnanimityNotMaintained(loop: SeqLoop[_], c1: SeqGuard[_], c2: SeqGuard[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      loop.blame.blame(LoopUnanimityNotMaintained(c1, c2))
  }
}

case class EncodeSeqBranchUnanimity[Pre <: Generation]() extends Rewriter[Pre] {

  val guardSucc = SuccessionMap[SeqGuard[Pre], Variable[Post]]()

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch@SeqBranch(guards, yes, no) =>
      implicit val o = statement.o
      /*
      for each c in conds:
        bool bc = c;

      for each subsequent c1, c2 in conds:
        assert c1 == c2

      boolean cond = foldStar(succ(c))

      if (cons) dispatch(yes) dispatch(no)
       */
      val assignments: Seq[Assign[Post]] = guards.map { guard =>
        guardSucc(guard) = new Variable(TBool())(guard.o.where(name = "g"))
        assignLocal(guardSucc(guard).get, dispatch(guard.condition))
      }

      val assertions: Seq[Assert[Post]] = (0 until guards.length - 1).map { i =>
        val c1 = guards(i)
        val c2 = guards(i + 1)
        Assert(guardSucc(c1).get === guardSucc(c2).get)(ForwardBranchUnanimity(branch, c1, c2))
      }

      val majorCond = new Variable[Post](TBool())
      val majorAssign: Assign[Post] = assignLocal[Post](
        majorCond.get,
        foldAnd(guards.map(guard => guardSucc(guard).get))
      )

      val finalIf: Branch[Post] = Branch[Post](Seq(
        (majorCond.get, dispatch(yes)),
        (tt, no.map(dispatch).getOrElse(Block(Seq())))
      ))

      Scope(guards.map(guardSucc(_)) :+ majorCond, Block(
        assignments ++
        assertions :+
        majorAssign :+
        finalIf
      ))

    case loop@SeqLoop(guards, contract, body) =>
      implicit val o = statement.o

      val assignments: Seq[Assign[Post]] = guards.map { guard =>
        guardSucc(guard) = new Variable(TBool())(guard.o.where(name = "g"))
        assignLocal(guardSucc(guard).get, dispatch(guard.condition))
      }

      val establishAssertions: Seq[Assert[Post]] = (0 until guards.length - 1).map { i =>
        val c1 = guards(i)
        val c2 = guards(i + 1)
        Assert(guardSucc(c1).get === guardSucc(c2).get)(ForwardLoopUnanimityNotEstablished(loop, c1, c2))
      }

      val maintainAssertions: Seq[Assert[Post]] = (0 until guards.length - 1).map { i =>
        val c1 = guards(i)
        val c2 = guards(i + 1)
        Assert(guardSucc(c1).get === guardSucc(c2).get)(ForwardLoopUnanimityNotMaintained(loop, c1, c2))
      }

      val combinedCond = new Variable[Post](TBool())(loop.o.where(name = "combined"))
      val combinedAssign: Assign[Post] = assignLocal[Post](
        combinedCond.get,
        foldAnd(guards.map(guard => guardSucc(guard).get))
      )

      val finalLoop: Loop[Post] = Loop(
        Block(assignments ++ establishAssertions :+ combinedAssign),
        combinedCond.get,
        Block(assignments ++ maintainAssertions :+ combinedAssign),
        dispatch(contract),
        dispatch(body)
      )

      Scope(guards.map(guardSucc(_)) :+ combinedCond, finalLoop)

    case statement => rewriteDefault(statement)
  }
}

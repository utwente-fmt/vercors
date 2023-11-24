package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{AssertFailed, Blame, BranchUnanimityFailed, LoopUnanimityNotEstablished, LoopUnanimityNotMaintained, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.EncodeSeqBranchUnanimity.{ForwardBranchUnanimity, ForwardLoopUnanimityNotEstablished, ForwardLoopUnanimityNotMaintained}

import scala.collection.mutable

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

  val currentLoop = ScopedStack[SeqLoop[Pre]]()

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case branch @ SeqBranch(guards, yes, no) =>
      implicit val o = statement.o

      val assertions: Block[Post] = Block(guards.indices.init.map { i =>
        Assert(rewriteGuard(guards(i)) === rewriteGuard(guards(i + 1)))(
          ForwardBranchUnanimity(branch, guards(i), guards(i + 1)))
      })

      Block(Seq(
        assertions,
        Branch[Post](
          Seq((foldAnd(guards.map(rewriteGuard)), dispatch(yes))) ++
            no.map { no => Seq((tt[Post], dispatch(no))) }.getOrElse(Nil))
      ))

    case loop @ SeqLoop(guards, contract, body) =>
      implicit val o = statement.o

      val establishAssertions: Statement[Post] = Block(guards.indices.init.map { i =>
        Assert(rewriteGuard(guards(i)) === rewriteGuard(guards(i + 1)))(
          ForwardLoopUnanimityNotEstablished(loop, guards(i), guards(i + 1)))
      })

      val maintainAssertions: Statement[Post] = Block(guards.indices.init.map { i =>
        Assert(rewriteGuard(guards(i)) === rewriteGuard(guards(i + 1)))(
          ForwardLoopUnanimityNotMaintained(loop, guards(i), guards(i + 1)))
      })

      val finalLoop: Loop[Post] = Loop(
        establishAssertions,
        foldAnd(guards.map(rewriteGuard)),
        maintainAssertions,
        currentLoop.having(loop) {
          dispatch(contract)
        },
        dispatch(body)
      )

      finalLoop

    case statement => rewriteDefault(statement)
  }

  def rewriteGuard(guard: SeqGuard[Pre]): Expr[Post] = dispatch(guard.condition)

  def allEqual[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    foldAnd[G](exprs.indices.init.map(i => exprs(i) === exprs(i + 1)))

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] = (currentLoop.topOption, contract) match {
    case (Some(loop), inv: LoopInvariant[Pre]) =>
      implicit val o = contract.o
      inv.rewrite(invariant = dispatch(inv.invariant) &* allEqual(loop.guards.map(rewriteGuard)))
    case (Some(loop), inv @ IterationContract(requires, ensures, _)) =>
      implicit val o = contract.o
      inv.rewrite(
        requires = dispatch(requires) &* allEqual(loop.guards.map(rewriteGuard)),
        ensures = dispatch(ensures) &* allEqual(loop.guards.map(rewriteGuard)))
    case _ => rewriteDefault(contract)
  }
}

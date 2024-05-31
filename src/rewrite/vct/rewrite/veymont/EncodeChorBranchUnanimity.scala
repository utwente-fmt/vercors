package vct.rewrite.veymont

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{
  AssertFailed,
  Blame,
  BranchUnanimityFailed,
  LoopUnanimityNotEstablished,
  LoopUnanimityNotMaintained,
  Origin,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError
import vct.rewrite.veymont.EncodeChorBranchUnanimity.{
  ForwardBranchUnanimity,
  ForwardLoopUnanimityNotEstablished,
  ForwardLoopUnanimityNotMaintained,
}

import scala.collection.mutable

object EncodeChorBranchUnanimity extends RewriterBuilder {
  override def key: String = "encodeSeqBranchUnanimity"
  override def desc: String =
    "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in seq_program nodes."

  case class ForwardBranchUnanimity(branch: Branch[_], c1: Expr[_], c2: Expr[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit = ???
    // TODO (RR): Repair blames -- branch.blame.blame(BranchUnanimityFailed(c1, c2))
  }

  case class ForwardLoopUnanimityNotEstablished(
      loop: Loop[_],
      c1: Expr[_],
      c2: Expr[_],
  ) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit = ???
    //  TODO (RR): Repair blames    loop.blame.blame(LoopUnanimityNotEstablished(c1, c2))
  }

  case class ForwardLoopUnanimityNotMaintained(
      loop: Loop[_],
      c1: Expr[_],
      c2: Expr[_],
  ) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit = ???
    // TODO (RR): Repair blames      loop.blame.blame(LoopUnanimityNotMaintained(c1, c2))
  }
}

case class EncodeChorBranchUnanimity[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] {

  val currentLoop = ScopedStack[ChorStatement[Pre]]()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) { super.dispatch(chor) }
      case _ => super.dispatch(decl)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case InChor(_, c @ ChorStatement(branch: Branch[Pre])) =>
        implicit val o = statement.o
        val guards = c.guards
        val assertions: Block[Post] = Block(guards.indices.init.map { i =>
          Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
            ForwardBranchUnanimity(branch, guards(i), guards(i + 1))
          )
        })

        Block(Seq(assertions, super.dispatch(branch)))

      case InChor(_, c @ ChorStatement(loop: Loop[Pre])) =>
        implicit val o = statement.o
        val guards = c.guards
        val establishAssertions: Statement[Post] = Block(
          guards.indices.init.map { i =>
            Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
              ForwardLoopUnanimityNotEstablished(loop, guards(i), guards(i + 1))
            )
          }
        )

        val maintainAssertions: Statement[Post] = Block(
          guards.indices.init.map { i =>
            Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
              ForwardLoopUnanimityNotMaintained(loop, guards(i), guards(i + 1))
            )
          }
        )

        val finalLoop: Loop[Post] = loop.rewrite(
          init = establishAssertions,
          update = maintainAssertions,
          contract = currentLoop.having(c) { dispatch(loop.contract) },
        )

        finalLoop

      case statement => statement.rewriteDefault()
    }

  def allEqual[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    foldAnd[G](exprs.indices.init.map(i => exprs(i) === exprs(i + 1)))

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    (currentLoop.topOption, contract) match {
      case (Some(c), inv: LoopInvariant[Pre]) =>
        implicit val o = contract.o
        inv.rewrite(invariant =
          dispatch(inv.invariant) &* allEqual(c.guards.map(dispatch))
        )
      case (Some(c), inv @ IterationContract(requires, ensures, _)) =>
        implicit val o = contract.o
        inv.rewrite(
          requires = dispatch(requires) &* allEqual(c.guards.map(dispatch)),
          ensures = dispatch(ensures) &* allEqual(c.guards.map(dispatch)),
        )
      case _ => contract.rewriteDefault()
    }
}

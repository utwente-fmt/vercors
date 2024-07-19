package vct.rewrite.veymont.verification

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.veymont.VeymontContext

object EncodeChorBranchUnanimity extends RewriterBuilderArg[Boolean] {
  override def key: String = "encodeChorBranchUnanimity"
  override def desc: String =
    "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in choreographies."

  case class ForwardBranchUnanimity(
      chor: ChorStatement[_],
      c1: Expr[_],
      c2: Expr[_],
  ) extends Blame[AssertFailed] {
    require(chor.inner match { case _: Branch[_] => true; case _ => false })
    override def blame(error: AssertFailed): Unit =
      chor.blame.blame(BranchUnanimityFailed(c1, c2))
  }

  case class ForwardLoopUnanimityNotEstablished(
      chor: ChorStatement[_],
      c1: Expr[_],
      c2: Expr[_],
  ) extends Blame[AssertFailed] {
    require(chor.inner match { case _: Loop[_] => true; case _ => false })
    override def blame(error: AssertFailed): Unit =
      chor.blame.blame(LoopUnanimityNotEstablished(c1, c2))
  }

  case class ForwardLoopUnanimityNotMaintained(
      chor: ChorStatement[_],
      c1: Expr[_],
      c2: Expr[_],
  ) extends Blame[AssertFailed] {
    require(chor.inner match { case _: Loop[_] => true; case _ => false })
    override def blame(error: AssertFailed): Unit =
      chor.blame.blame(LoopUnanimityNotMaintained(c1, c2))
  }
}

case class EncodeChorBranchUnanimity[Pre <: Generation](enabled: Boolean)
    extends Rewriter[Pre] with VeymontContext[Pre] {

  case class IdentityRewriter[Pre <: Generation]() extends Rewriter[Pre] {}

  val currentLoop = ScopedStack[Loop[Pre]]()

  override def dispatch(program: Program[Pre]): Program[Post] =
    if (enabled)
      super.dispatch(program)
    else
      IdentityRewriter().dispatch(program)

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
        val guards = unfoldStar(branch.cond)
        val assertions: Block[Post] = Block(guards.indices.init.map { i =>
          Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
            ForwardBranchUnanimity(c, guards(i), guards(i + 1))
          )
        })

        Block(Seq(assertions, super.dispatch(branch)))

      case InChor(_, c @ ChorStatement(loop: Loop[Pre])) =>
        implicit val o = statement.o
        val guards = unfoldStar(loop.cond)
        val establishAssertions: Statement[Post] = Block(
          guards.indices.init.map { i =>
            Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
              ForwardLoopUnanimityNotEstablished(c, guards(i), guards(i + 1))
            )
          }
        )

        val maintainAssertions: Statement[Post] = Block(
          guards.indices.init.map { i =>
            Assert(dispatch(guards(i)) === dispatch(guards(i + 1)))(
              ForwardLoopUnanimityNotMaintained(c, guards(i), guards(i + 1))
            )
          }
        )

        val finalLoop: Loop[Post] = loop.rewrite(
          init = establishAssertions,
          update = maintainAssertions,
          contract = currentLoop.having(loop) { dispatch(loop.contract) },
        )

        finalLoop

      case statement => statement.rewriteDefault()
    }

  def allEqual[G](exprs: Seq[Expr[G]])(implicit o: Origin): Expr[G] =
    foldAnd[G](exprs.indices.init.map(i => exprs(i) === exprs(i + 1)))

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    (currentLoop.topOption, contract) match {
      case (Some(loop), inv: LoopInvariant[Pre]) =>
        implicit val o = contract.o
        val guards = unfoldStar(loop.cond)
        inv.rewrite(invariant =
          dispatch(inv.invariant) &* allEqual(guards.map(dispatch))
        )
      case (Some(loop), inv @ IterationContract(requires, ensures, _)) =>
        implicit val o = contract.o
        val guards = unfoldStar(loop.cond)
        inv.rewrite(
          requires = dispatch(requires) &* allEqual(guards.map(dispatch)),
          ensures = dispatch(ensures) &* allEqual(guards.map(dispatch)),
        )
      case _ => contract.rewriteDefault()
    }
}

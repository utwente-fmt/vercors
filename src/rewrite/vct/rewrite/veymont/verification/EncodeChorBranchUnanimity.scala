package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  ChorStatement,
  Declaration,
  Expr,
  LoopContract,
  EndpointExpr,
  CommTargetEndpoint,
  CommTargetIndex,
  Declaration,
  Choreography,
  Program,
  Loop,
  Branch,
  Statement,
  Block,
  Assert,
  LoopInvariant,
  IterationContract,
  CommunicateTarget,
  RangeBinder,
  CommTargetRange,
  Select,
  Variable,
  TInt,
  TBool,
}
import vct.col.origin._
import vct.col.ref.{DirectRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.veymont.VeymontContext
import vct.rewrite.veymont.verification.EncodeChorBranchUnanimity.{
  ForwardBranchUnanimity,
  ForwardLoopUnanimityNotEstablished,
  ForwardLoopUnanimityNotMaintained,
}

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
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  case class IdentityRewriter[Pre <: Generation]() extends Rewriter[Pre] {}

  val currentLoop = ScopedStack[Loop[Pre]]()

  override def dispatch(program: Program[Pre]): Program[Post] =
    if (enabled) {
      mappings.program = program
      super.dispatch(program)
    } else {
      logger.warn(
        "Branch unanimity turned off. User should do an informal deadlock freedom proof."
      )
      IdentityRewriter().dispatch(program)
    }

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

  def unanimous(exprs: Expr[Pre]): Expr[Post] = {
    val subExprs: Seq[EndpointExpr[Pre]] = unfoldAny(exprs).map {
      case expr: EndpointExpr[Pre] => expr
      case _ => ???
    }
    require(subExprs.nonEmpty)
    val alphas = subExprs.map(_.endpoint)
    // This is either an endpoint e or an indexed family F[i], meaning a singular endpoint
    // All sub-conditions in the total condition will be checked to be equal to the condition of this endpoint
    // For aesthetic purposes, we try to pick a singular endpoint from the list of alphas, but this is not strictly
    // necessary. You could pick any of the endpoints participating in the condition.
    val grounder = {
      val singularCandidate = alphas.collectFirst {
        case endpoint: CommTargetEndpoint[Pre] => endpoint
        case index: CommTargetIndex[Pre] => index
      }
      singularCandidate match {
        case Some(singular) => singular
        case None =>
          // No singular candidate in condition. Since the condition is nonempty,
          // and there were no singular CommunicationTarget, it is guaranteed that the first is a CommTargetRange.
          // We just pick that
          alphas.head
      }
    }
    // Drop all endpoint exprs that mention the grounder. This is incomplete, as we do only a syntactic check. But in the case
    // of a singular CommunicateTarget it reduces the length of the filteredSubExprs list nicely.
    val filteredSubExprs = subExprs.filter(_.endpoint != grounder)
    val filteredAlphas = filteredSubExprs.map { _.endpoint }
    implicit val o: Origin = TraceOrigin()
    val groundCondition =
      ??? // Kind of need to apply ground here, but that requires a groundCondition. I just want to apply the partial projection operator here... Hmm
    foldAny1(exprs.t)(filteredAlphas.map { alpha =>
      ground(filteredSubExprs, grounder, alpha)
    })
  }

  def ground(
      groundCondition: Expr[Post],
      exprs: Seq[EndpointExpr[Pre]],
      mainTarget: CommunicateTarget[Pre],
  )(implicit o: Origin): Expr[Post] =
    groundCondition === foldAnd(exprs.map { expr =>
      narrowCommunicateTarget(expr.endpoint, mainTarget).map { subTarget =>
        expr.rewrite(endpoint = subTarget)
      }.collect { case Some(expr) => expr }
    })

  // Either narrows a target in accordance with some context, or returns None if the two targets are
  // not related - e.g. when narrowing an endpoint to the context of a endpoint range.
  def narrowCommunicateTarget(
      target: CommunicateTarget[Pre],
      context: CommunicateTarget[Pre],
  ): Option[CommunicateTarget[Post]] =
    (target, context) match {
      case (target, context) if target.isSingle && target == context =>
        Some(dispatch(target))
      case (
            CommTargetRange(Ref(a), RangeBinder(binder, fLow, fHigh)),
            CommTargetRange(Ref(b), RangeBinder(_, gLow, gHigh)),
          ) if a == b =>
        implicit val o: Origin = TraceOrigin()
        Some(CommTargetRange[Post](
          succ(a),
          RangeBinder(
            variables.dispatch(binder),
            max(fLow.rewriteDefault(), gLow.rewriteDefault()),
            min(fHigh.rewriteDefault(), gHigh.rewriteDefault()),
          ),
        ))
      case (
            CommTargetIndex(ref @ Ref(a), i),
            CommTargetRange(Ref(b), RangeBinder(_, low, high)),
          ) if a == b =>
        implicit val o: Origin = TraceOrigin()
        // Implement support for this case by simulating the case of F[i] as a range F[i' := i .. i + 1]
        // (in this cased, the i' variable does not need to be used: it is directly equal to i, so i can safely be used instead)
        // (except that i is an expr and i' a var, but that's irrelevant here)
        Some(CommTargetRange[Post](
          endpoints.dispatch(ref),
          RangeBinder(
            new Variable(TInt()),
            max(dispatch(i), dispatch(low)),
            min(dispatch(i) + const(1), dispatch(high)),
          ),
        ))

    }

  def compareFun(
      op: (Expr[Post], Expr[Post]) => Expr[Post]
  )(implicit o: Origin): Function[Post] = {
    val x = new Variable[Post](TInt())(o.sourceName("x"))
    val y = new Variable[Post](TInt())(o.sourceName("y"))
    function[Post](
      args = Seq(x, y),
      returnType = TInt(),
      body = Some(Select[Post](op(x.get, y.get), x.get, y.get)),
      blame = TrivialContract(),
      contractBlame = TrueSatisfiable,
    ).declare()
  }

  lazy val minFun = {
    implicit val o: Origin = TraceOrigin().sourceName("min")
    compareFun((x, y) => x < y)
  }
  lazy val maxFun = {
    implicit val o: Origin = TraceOrigin().sourceName("max")
    compareFun((x, y) => x > y)
  }

  def min(a: Expr[Post], b: Expr[Post])(implicit o: Origin): Expr[Post] =
    functionInvocation(
      ref = minFun.ref,
      args = Seq(a, b),
      blame = TrivialContract(),
    )
  def max(a: Expr[Post], b: Expr[Post])(implicit o: Origin): Expr[Post] =
    functionInvocation(
      ref = maxFun.ref,
      args = Seq(a, b),
      blame = TrivialContract(),
    )

}

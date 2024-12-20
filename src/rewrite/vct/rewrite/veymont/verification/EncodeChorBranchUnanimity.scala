package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Assert,
  Local,
  Endpoint,
  Block,
  Branch,
  ChorStatement,
  Choreography,
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  CommunicateTarget,
  Declaration,
  EndpointExpr,
  Expr,
  Function,
  IterationContract,
  Loop,
  LoopContract,
  LoopInvariant,
  Node,
  Program,
  RangeBinder,
  Select,
  Statement,
  TBool,
  TInt,
  Variable,
  And,
  Or,
  BooleanValue,
}
import vct.col.compare.Compare
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

import scala.collection.mutable

object EncodeChorBranchUnanimity extends RewriterBuilderArg[Boolean] {
  override def key: String = "encodeChorBranchUnanimity"
  override def desc: String =
    "Encodes the branch unanimity requirement imposed by VeyMont on branches and loops in choreographies."

  case class ForwardBranchUnanimity(chor: ChorStatement[_], c1: Node[_])
      extends Blame[AssertFailed] {
    require(chor.inner match { case _: Branch[_] => true; case _ => false })
    override def blame(error: AssertFailed): Unit =
      chor.blame.blame(BranchUnanimityFailed1(c1))
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

// Adds assertions to check branch unanimity to the program. Not sure if this is the best way, as some kind of partial
// projection hack is needed to do the encoding of the unanimity before the actual projection is done. Might as well
// inline it in the EncodeChoreography pass.
case class EncodeChorBranchUnanimity[Pre <: Generation](enabled: Boolean)
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {

  case class IdentityRewriter[Pre <: Generation]() extends Rewriter[Pre] {}

  val currentLoop = ScopedStack[Loop[Pre]]()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    if (enabled) {
      mappings.program = program
      super.dispatch(program)
    } else {
      logger.warn(
        "Branch unanimity turned off. User should do an informal deadlock freedom proof."
      )
      IdentityRewriter().dispatch(program)
    }
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
        Block(Seq(
          Assert(unanimous(branch.cond))(
            ForwardBranchUnanimity(c, branch.cond)
          ),
          super.dispatch(branch),
        ))

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

  def getAlphas(expr: Expr[Pre]): Seq[CommunicateTarget[Pre]] =
    expr match {
      case And(l, r) => Seq(l, r).flatMap(getAlphas)
      case EndpointExpr(alpha, _) => Seq(alpha)
    }

  def sort(target: CommunicateTarget[Pre]): Endpoint[Pre] =
    target match {
      case CommTargetEndpoint(Ref(e)) => e
      case CommTargetIndex(Ref(e), _) => e
      case CommTargetRange(Ref(e), _) => e
    }

  val substitutions: ScopedStack[Map[Expr[Pre], Expr[Post]]] = ScopedStack(
    Map.empty
  )

  // Here, r can only be a singular endpoint, not a range
  // I fully expanded the match statement to make sure we cover every single case
  def partialProject(expr: Expr[Pre], r: CommunicateTarget[Pre]): Expr[Post] =
    (expr, r) match {
      case (expr @ And(p, q), r) =>
        // Distribute over and
        implicit val o: Origin = expr.o
        partialProject(p, r) |&&| partialProject(q, r)
      case (
            expr @ EndpointExpr(alpha @ CommTargetEndpoint(Ref(a)), inner),
            CommTargetEndpoint(Ref(b)),
          ) if a == b =>
        // a & b match, we keep the endpoint expr
        expr.rewrite(endpoint = alpha.rewriteDefault(), expr = dispatch(inner))
      case (
            EndpointExpr(CommTargetIndex(Ref(f), i), inner),
            CommTargetIndex(Ref(g), j),
          ) if f == g =>
        implicit val o: Origin = expr.o
        // evaluate if the indices are the same
        (dispatch(i) === dispatch(j)) ==> dispatch(inner)
      case (
            EndpointExpr(
              CommTargetRange(Ref(f), RangeBinder(v, low, high)),
              inner,
            ),
            CommTargetIndex(Ref(g), j),
          ) =>
        implicit val o: Origin = expr.o
        // Evaluate if j is within range. Since j should be a mathematically expression (no method calls &
        // no heap dereferences) it's safe to duplicate it all over the place. Though it might be bad for prover performance.
        // We don't check for this constraint, though.
        ((dispatch(low) <= dispatch(j)) && (dispatch(j) < dispatch(high))) ==>
          substitutions.having(substitutions.top.updated(v.get, dispatch(j))) {
            dispatch(inner)
          }
      case (EndpointExpr(alpha, _), r) =>
        // If none of the cases match, the endpoints of alpha & r cannot be the same, as otherwise there
        // should have been a matching case.
        assert(sort(alpha) != sort(r))
        BooleanValue(true)(expr.o)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case v: Local[Pre] if substitutions.top.contains(v) =>
        substitutions.top(v)
      case _ => expr.rewriteDefault()
    }

  def ground(
      expr: Expr[Pre],
      alpha: CommunicateTarget[Pre],
      b: Expr[Post],
  ): Expr[Post] = {
    implicit val o: Origin = expr.o
    alpha match {
      case r @ (_: CommTargetEndpoint[Pre] | _: CommTargetIndex[Pre]) =>
        partialProject(expr, r) |===| b
      case target @ CommTargetRange(f, RangeBinder(oldV, low, high)) =>
        variables.scope {
          val newTarget = target.rewriteDefault()
          val v = newTarget.range.binder.get
          EndpointExpr(
            newTarget,
            ((dispatch(low) <= v) && (v < dispatch(high))) ==>
              partialProject(expr, CommTargetIndex(f, oldV.get)) |===| b,
          )
        }
    }
  }

  def unanimous(cond: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = cond.o
    val alphas = hre.util.FuncTools.unique(getAlphas(cond))
    Or(
      alphas.map { alpha => ground(cond, alpha, tt) }.reduce { (l, r) =>
        l |&&| r
      },
      alphas.map { alpha => ground(cond, alpha, ff) }.reduce { (l, r) =>
        l |&&| r
      },
    )
  }

//  def unanimousOld(
//      exprs: Expr[Pre]
//  ): Seq[(Expr[Post], (CommunicateTarget[Pre], CommunicateTarget[Pre]))] = {
//    implicit val o: Origin = TraceOrigin()
//    val subExprs: Seq[EndpointExpr[Pre]] = unfoldAny(exprs).map {
//      case expr: EndpointExpr[Pre] => expr
//      case _ => ???
//    }
//    require(subExprs.nonEmpty)
//    val alphas = subExprs.map(_.endpoint)
//    // `grounder` is either a singular endpoint e or an indexed family F[i], meaning a singular endpoint. It is the
//    // endpoint of which the condition will be compared to all other conditions.
//    // For aesthetic purposes, we try to pick a singular endpoint from the list of alphas, but this is not strictly
//    // necessary. Mostly because it might make the grounded condition smaller.
//    // You could pick any of the endpoints participating in the condition, in theory.
//    val grounder = alphas.find(_.isSingle).getOrElse(alphas.head)
//    val groundCondition = partialProject(exprs, grounder)
//    // Get all subexpressions that don't mention the grounder. This is not precise, as we do only a syntactic check.
//    // But in the case of a singular CommunicateTarget it reduces the length of the filteredSubExprs list nicely.
//    val filteredExprs = subExprs
//      // Keep all subexpressions for which the alphas are neither identity-equal, nor isomorphic
//      .filter(expr =>
//        expr.endpoint != grounder
//        /* TODO (RR): Would like to include isomorphism somehow, but "just" doing it like below is wrong.
//                      I think you need to do the isomorphism check on the contents of CommunicateTarget,
//                      but not on the endpoints with it.*/
////          !Compare.isIsomorphic(expr.endpoint, grounder)
//      )
//    // If there are no filtered exprs, it means that only one endpoint is involved in the branch, meaning there is nothing to check
//    if (filteredExprs.isEmpty)
//      return Seq()
//    // This is the expression that the non-ground endpoints will evaluate
//    val filteredExpr = foldAny1(TBool())(filteredExprs)
//    // Apply the partial projection to the filtered expression for each other participant, and
//    // construct the aggregated branch condition
//    val nonGroundEndpoints =
//      // LinkedHashSet because it is order preserving
//      mutable.LinkedHashSet.from(filteredExprs.map(_.endpoint)).toSeq
//    nonGroundEndpoints.map { alpha =>
//      (
//        groundCondition === partialProject(filteredExpr, alpha),
//        (grounder, alpha),
//      )
//    }
//  }
//
//  // Does a lazy/partial projection by only modifying the endpoint expr nodes, leaving everything inside those expressions intact
//  def partialProject(expr: Expr[Pre], mainTarget: CommunicateTarget[Pre])(
//      implicit o: Origin
//  ): Expr[Post] =
//    foldAny1(TBool())(
//      unfoldAny(expr).map {
//        // Should be statically guaranteed that you only get EndpointExprs at this point
//        case expr: EndpointExpr[Pre] =>
//          narrowCommunicateTarget(expr.endpoint, mainTarget).map { subTarget =>
//            expr.rewrite(endpoint = subTarget)
//          }
//        case _ => ???
//      }.collect { case Some(expr) => expr }
//    )
//
//  // Either narrows a target in accordance with some context, or returns None if the two targets are
//  // not related - e.g. when narrowing an endpoint to the context of a endpoint range.
//  def narrowCommunicateTarget(
//      target: CommunicateTarget[Pre],
//      context: CommunicateTarget[Pre],
//  ): Option[CommunicateTarget[Post]] =
//    (target, context) match {
//      case (target, context) if target.isSingle && target == context =>
//        Some(dispatch(target))
//      case (target, context)
//          if target.isSingle && context.isSingle && target != context =>
//        None
//      case (
//            CommTargetRange(Ref(a), RangeBinder(binder, fLow, fHigh)),
//            CommTargetRange(Ref(b), RangeBinder(_, gLow, gHigh)),
//          ) if a == b =>
//        implicit val o: Origin = TraceOrigin()
//        Some(CommTargetRange[Post](
//          succ(a),
//          RangeBinder(
//            variables.dispatch(binder),
//            max(fLow.rewriteDefault(), gLow.rewriteDefault()),
//            min(fHigh.rewriteDefault(), gHigh.rewriteDefault()),
//          ),
//        ))
//      case (
//            CommTargetIndex(ref @ Ref(a), i),
//            CommTargetRange(Ref(b), RangeBinder(_, low, high)),
//          ) if a == b =>
//        implicit val o: Origin = TraceOrigin()
//        // Implement support for this case by simulating the case of F[i] as a range F[i' := i .. i + 1]
//        // (in this cased, the i' variable does not need to be used: it is directly equal to i, so i can safely be used instead)
//        // (except that i is an expr and i' a var, but that's irrelevant here)
//        Some(CommTargetRange[Post](
//          endpoints.dispatch(ref),
//          RangeBinder(
//            new Variable(TInt()),
//            max(dispatch(i), dispatch(low)),
//            min(dispatch(i) + const(1), dispatch(high)),
//          ),
//        ))
//      /* TODO (RR): What about the case when context if F[i} and target is a range? Do we then just collapse the range into the unit range of F[i]?
//                    Probably use min/max again to accomodate for when i is outside the range. */
//
//    }
//
//  def compareFun(
//      op: (Expr[Post], Expr[Post]) => Expr[Post]
//  )(implicit o: Origin): Function[Post] = {
//    val x = new Variable[Post](TInt())(o.sourceName("x"))
//    val y = new Variable[Post](TInt())(o.sourceName("y"))
//    function[Post](
//      args = Seq(x, y),
//      returnType = TInt(),
//      body = Some(Select[Post](op(x.get, y.get), x.get, y.get)),
//      blame = TrivialContract(),
//      contractBlame = TrueSatisfiable,
//    ).declare()
//  }
//
//  lazy val minFun = {
//    implicit val o: Origin = TraceOrigin().sourceName("min")
//    compareFun((x, y) => x < y)
//  }
//  lazy val maxFun = {
//    implicit val o: Origin = TraceOrigin().sourceName("max")
//    compareFun((x, y) => x > y)
//  }
//
//  def min(a: Expr[Post], b: Expr[Post])(implicit o: Origin): Expr[Post] =
//    functionInvocation(
//      ref = minFun.ref,
//      args = Seq(a, b),
//      blame = TrivialContract(),
//    )
//  def max(a: Expr[Post], b: Expr[Post])(implicit o: Origin): Expr[Post] =
//    functionInvocation(
//      ref = maxFun.ref,
//      args = Seq(a, b),
//      blame = TrivialContract(),
//    )

}

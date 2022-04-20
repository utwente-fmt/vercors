package vct.col.newrewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.util.{Extract, Substitute}
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
case object ParBlockEncoder extends RewriterBuilder {
  override def key: String = "parBlock"
  override def desc: String = "Translate parallel blocks into methods and generate checks for them."

  def regionName(region: ParRegion[_]): String = region match {
    case ParParallel(regions) => "par_$" + regions.map(regionName).mkString("_") + "$"
    case ParSequential(regions) => "seq_$" + regions.map(regionName).mkString("_") + "$"
    case block: ParBlock[_] => block.o.preferredName
  }

  case class ParRegionImpl(region: ParRegion[_]) extends Origin {
    override def preferredName: String = "do_" + regionName(region)
    override def shortPosition: String = region.o.shortPosition
    override def context: String = region.o.context
    override def inlineContext: String = region.o.inlineContext
  }

  case class ParBlockCheck(block: ParBlock[_]) extends Origin {
    override def preferredName: String =
      "check_" + regionName(block)

    override def shortPosition: String = block.o.shortPosition
    override def context: String = block.o.context
    override def inlineContext: String = block.o.inlineContext
  }

  case object ParImpl extends Origin {
    override def preferredName: String = "unknown"
    override def shortPosition: String = "generated"
    override def context: String = s"[At node generated for the implementation of parallel blocks]"
    override def inlineContext: String = s"[Implementation of parallel block]"
  }

  case class ParPreconditionPostconditionFailed(region: ParRegion[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParPreconditionPreconditionFailed(region: ParRegion[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParPostconditionImplementationFailure(block: ParBlock[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case PostconditionFailed(_, failure, _) =>
        block.blame.blame(ParBlockPostconditionFailed(failure, block))
      case ctx: ContextEverywhereFailedInPost =>
        PanicBlame("the generated method for a parallel block thread does not include context_everywhere clauses.").blame(ctx)
      case SignalsFailed(_, _) =>
        block.blame.blame(ParBlockMayNotThrow(block))
      case ExceptionNotInSignals(_) =>
        block.blame.blame(ParBlockMayNotThrow(block))
    }
  }

  case class EmptyHintCannotThrow(inner: Blame[PostconditionFailed]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case err: PostconditionFailed => inner.blame(err)
      case _: ContextEverywhereFailedInPost =>
        PanicBlame("A procedure generated to prove an implication does not have context_everywhere clauses.").blame(error)
      case _: SignalsFailed | _: ExceptionNotInSignals =>
        PanicBlame("A procedure that proves an implication, of which the body is the nop statement cannot throw an exception.").blame(error)
    }
  }

  case class ParBlockNotInjective(block: ParBlock[_], expr: Expr[_]) extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      block.blame.blame(ParPredicateNotInjective(block, expr))
  }
}

case class ParBlockEncoder[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  import ParBlockEncoder._

  val invariants: ScopedStack[Expr[Pre]] = ScopedStack()
  val parDecls: mutable.Map[ParBlockDecl[Pre], ParBlock[Pre]] = mutable.Map()

  def quantify(block: ParBlock[Pre], expr: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
    val quantVars = block.iters.map(_.variable).map(v => v -> new Variable[Pre](v.t)(v.o)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local[Pre](l.ref) -> Local[Pre](r.ref) }.toMap[Expr[Pre], Expr[Pre]]).dispatch(expr)
    block.iters.foldLeft(body)((body, iter) => {
      val v = quantVars(iter.variable)
      Starall(Seq(v), Nil, (iter.from <= v.get && v.get < iter.to) ==> body)(ParBlockNotInjective(block, expr))
    })
  }

  def proveImplies(blame: Blame[PostconditionFailed], antecedent: Expr[Post], consequent: Expr[Post])(implicit origin: Origin): Unit = {
    proveImplies(EmptyHintCannotThrow(blame), antecedent, consequent, Block(Nil))
  }

  def proveImplies(blame: Blame[CallableFailure], antecedent: Expr[Post], consequent: Expr[Post], hint: Statement[Post])(implicit origin: Origin): Unit = {
    val (Seq(req, ens), bindings) =
      Extract.extract(antecedent, consequent)

    procedure[Post](
      blame = blame,
      requires = UnitAccountedPredicate(req),
      ensures = UnitAccountedPredicate(ens),
      args = bindings.keys.toSeq,
      body = Some(hint),
    ).declareDefault(this)
  }

  def requires(region: ParRegion[Pre], includingInvariant: Boolean = false)(implicit o: Origin): Expr[Pre] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(requires(_, includingInvariant)))
    case ParSequential(regions) => regions.headOption.map(requires(_, includingInvariant)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      if(includingInvariant) block.context_everywhere &* quantify(block, block.requires) else quantify(block, block.requires)
  }

  def ensures(region: ParRegion[Pre], includingInvariant: Boolean = false)(implicit o: Origin): Expr[Pre] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(ensures(_, includingInvariant)))
    case ParSequential(regions) => regions.lastOption.map(ensures(_, includingInvariant)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      if(includingInvariant) block.context_everywhere &* quantify(block, block.ensures) else quantify(block, block.ensures)
  }

  val regionAsMethod: mutable.Map[ParRegion[Pre], (Procedure[Post], Seq[Expr[Post]])] = mutable.Map()

  def getRegionMethod(region: ParRegion[Pre]): (Procedure[Post], Seq[Expr[Post]]) = {
    implicit val o: Origin = ParImpl
    regionAsMethod.getOrElseUpdate(region, region match {
      case ParParallel(regions) =>
        val (Seq(req, ens, inv), vars) = Extract.extract[Pre](requires(region, includingInvariant = true), ensures(region, includingInvariant = true), foldStar(invariants.toSeq))
        val result = procedure[Post](
          blame = AbstractApplicable,
          args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
          requires = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* req) }),
          ensures = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* ens) }),
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.map(dispatch).toSeq)
      case ParSequential(regions) =>
        val (Seq(req, ens, inv), vars) = Extract.extract[Pre](requires(region, includingInvariant = true), ensures(region, includingInvariant = true), foldStar(invariants.toSeq))

        val result = procedure[Post](
          blame = AbstractApplicable,
          args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
          requires = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* req) }),
          ensures = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* ens) }),
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.map(dispatch).toSeq)
      case block: ParBlock[Pre] =>
        invariants.having(block.context_everywhere) {
          val (Seq(req, ens, inv), vars) = Extract.extract[Pre](requires(block), ensures(block), foldStar(invariants.toSeq))

          val result = procedure[Post](
            blame = AbstractApplicable,
            args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
            requires = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* req) }),
            ensures = UnitAccountedPredicate(freshSuccessionScope { dispatch(inv &* ens) }),
          )(ParRegionImpl(region))
          result.declareDefault(this)
          (result, vars.values.map(dispatch).toSeq)
        }
    })
  }

  def emitChecks(region: ParRegion[Pre]): Unit = region match {
    case ParParallel(regions) =>
      // The parallel composition of regions is automatically valid
      regions.foreach(emitChecks)
    case ParSequential(regions) =>
      // For sequential composition we verify that pairs of sequentially composed regions have a matching post- and precondition
      implicit val o: Origin = region.o
      regions.zip(regions.tail).foreach {
        case (left, right) =>
          proveImplies(
            ParPreconditionPostconditionFailed(right),
            freshSuccessionScope { dispatch(ensures(left, includingInvariant = true)) },
            freshSuccessionScope { dispatch(requires(right, includingInvariant = true)) }
          )
      }
      regions.foreach(emitChecks)
    case block @ ParBlock(decl, iters, ctx, req, ens, content) =>
      // For blocks we generate a separate check, by checking the contract for an indeterminate iteration
      parDecls(decl) = block
      decl.drop()
      implicit val o: Origin = region.o
      val extract = Extract[Pre]()

      val ranges = iters.map {
        case IterVariable(v, from, to) =>
          v.drop()
          from <= v.get && v.get < to
      }.map(extract.extract)

      val requires = extract.extract(req)
      val ensures = extract.extract(ens)
      val context = extract.extract(ctx)
      val invariant = extract.extract(foldStar(invariants.toSeq))

      val body = extract.extract(content)

      val vars = extract.finish()

      val args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) }

      logger.debug(s"At ${decl.o.preferredName}:")
      logger.debug(s"    - requires = $requires")
      logger.debug(s"    - ensures = $ensures")
      logger.debug(s"    - invariant = $invariant")
      logger.debug(s"    - ranges = ${foldAnd(ranges)}")
      logger.debug(s"    - context = $context")
      logger.debug("")

      val invariantHere = invariant &* context &* foldAnd(ranges)

      invariants.having(context &* foldAnd(ranges)) {
        procedure(
          blame = ParPostconditionImplementationFailure(block),
          args = args,
          requires = UnitAccountedPredicate(freshSuccessionScope { dispatch(invariantHere) } &* dispatch(requires)),
          ensures = UnitAccountedPredicate(freshSuccessionScope { dispatch(invariantHere) } &* dispatch(ensures)),
          body = Some(dispatch(body)),
        )(ParBlockCheck(block)).declareDefault(this)
      }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case parRegion @ ParStatement(impl) =>
      implicit val o: Origin = parRegion.o
      val (proc, args) = getRegionMethod(impl)
      emitChecks(impl)
      Eval(ProcedureInvocation[Post](proc.ref, args, Nil, Nil, Nil, Nil)(NoContext(ParPreconditionPreconditionFailed(impl))))

    case other => rewriteDefault(other)
  }
}

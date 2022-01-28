package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.util.{Extract, Substitute}
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable
case object ParBlockEncoder extends RewriterBuilder {
  def regionName(region: ParRegion[_]): String = region match {
    case ParParallel(regions) => "par_$" + regions.map(regionName).mkString("_") + "$"
    case ParSequential(regions) => "seq_$" + regions.map(regionName).mkString("_") + "$"
    case block: ParBlock[_] => block.o.preferredName
  }

  case class ParRegionImpl(region: ParRegion[_]) extends Origin {
    override def messageInContext(message: String): String =
      region.o.messageInContext(message)

    override def preferredName: String = regionName(region)
  }

  case class ParBlockCheck(block: ParBlock[_]) extends Origin {
    override def preferredName: String =
      "check_" + block.o.preferredName

    override def messageInContext(message: String): String =
      block.o.messageInContext(message)
  }

  case object ParImpl extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String =
      s"[At node generated for the implementation of parallel blocks]: $message"
  }

  case class ParInvariantCannotBeExhaled(invariant: ParInvariant[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      invariant.blame.blame(ParInvariantNotEstablished(error.failure, invariant))
  }

  case class ParBarrierPostconditionFailed(barrier: ParBarrier[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case PostconditionFailed(_, failure, _) =>
        barrier.blame.blame(ParBarrierInconsistent(failure, barrier))
      case ctx: ContextEverywhereFailedInPost =>
        PanicBlame("the generated method for a barrier proof does not include context_everywhere clauses.").blame(ctx)
      case _: SignalsFailed | _: ExceptionNotInSignals =>
        barrier.blame.blame(ParBarrierMayNotThrow(barrier))
    }
  }

  case class ParBarrierExhaleFailed(barrier: ParBarrier[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierNotEstablished(error.failure, barrier))
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
      case SignalsFailed(failure, _) =>
        block.blame.blame(ParBlockMayNotThrow(failure, block))
      case ExceptionNotInSignals(failure, _) =>
        block.blame.blame(ParBlockMayNotThrow(failure, block))
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
}

case class ParBlockEncoder[Pre <: Generation]() extends Rewriter[Pre] {
  import ParBlockEncoder._

  val invariants: ScopedStack[Expr[Post]] = ScopedStack()
  val parDecls: mutable.Map[ParBlockDecl[Pre], ParBlock[Pre]] = mutable.Map()

  def quantify(block: ParBlock[Pre], expr: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
    val quantVars = block.iters.map(_.variable).map(v => v -> new Variable[Pre](v.t)(v.o)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local[Pre](l.ref) -> Local[Pre](r.ref) }.toMap[Expr[Pre], Expr[Pre]]).dispatch(expr)
    block.iters.foldLeft(body)((body, iter) => {
      val v = quantVars(iter.variable)
      Starall(Seq(v), Nil, (iter.from <= v.get && v.get < iter.to) ==> body)
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

  def requires(region: ParRegion[Pre])(implicit o: Origin): Expr[Pre] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(requires))
    case ParSequential(regions) => regions.headOption.map(requires).getOrElse(tt)
    case block: ParBlock[Pre] => quantify(block, block.requires)
  }

  def ensures(region: ParRegion[Pre])(implicit o: Origin): Expr[Pre] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(ensures))
    case ParSequential(regions) => regions.headOption.map(ensures).getOrElse(tt)
    case block: ParBlock[Pre] => quantify(block, block.ensures)
  }

  val regionAsMethod: mutable.Map[ParRegion[Pre], (Procedure[Post], Seq[Expr[Post]])] = mutable.Map()

  def getRegionMethod(region: ParRegion[Pre]): (Procedure[Post], Seq[Expr[Post]]) = {
    implicit val o: Origin = ParImpl
    regionAsMethod.getOrElseUpdate(region, region match {
      case ParParallel(regions) =>
        val (Seq(req, ens), vars) = Extract.extract[Pre](requires(region), ensures(region))
        val result = procedure[Post](
          blame = AbstractApplicable,
          args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
          requires = UnitAccountedPredicate(dispatch(req)),
          ensures = UnitAccountedPredicate(dispatch(ens)),
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.map(dispatch).toSeq)
      case ParSequential(regions) =>
        val (Seq(req, ens), vars) = Extract.extract[Pre](requires(region), ensures(region))

        val result = procedure[Post](
          blame = AbstractApplicable,
          args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
          requires = UnitAccountedPredicate(dispatch(req)),
          ensures = UnitAccountedPredicate(dispatch(ens)),
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.map(dispatch).toSeq)
      case block: ParBlock[Pre] =>
        val (Seq(req, ens), vars) = Extract.extract[Pre](requires(block), ensures(block))

        val result = procedure[Post](
          blame = AbstractApplicable,
          args = collectInScope(variableScopes) { vars.keys.foreach(dispatch) },
          requires = UnitAccountedPredicate(dispatch(req)),
          ensures = UnitAccountedPredicate(dispatch(ens)),
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.map(dispatch).toSeq)
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
          proveImplies(ParPreconditionPostconditionFailed(right), dispatch(ensures(left)), dispatch(requires(right)))
      }
    case block @ ParBlock(decl, iters, req, ens, content) =>
      // For blocks we generate a separate check, by checking the contract for an indeterminate iteration
      parDecls(decl) = block
      decl.drop()
      implicit val o: Origin = region.o
      val extract = Extract[Post]()

      val ranges = iters.map {
        case IterVariable(v, from, to) =>
          val extractDummy = new Variable(dispatch(v.t))(v.o)
          successionMap(v) = extractDummy
          dispatch(from) <= dispatch(v.get) && dispatch(v.get) < dispatch(to)
      }.map(extract.extract)

      val requires = extract.extract(dispatch(req))
      val ensures = extract.extract(dispatch(ens))

      val body = extract.extract(dispatch(content))

      val vars = extract.finish()

      procedure(
        blame = ParPostconditionImplementationFailure(block),
        args = vars.keys.toSeq,
        requires = UnitAccountedPredicate(AstBuildHelpers.foldStar(ranges) &* requires),
        ensures = UnitAccountedPredicate(ensures),
        body = Some(body),
      )(ParBlockCheck(block))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case parInv @ ParInvariant(_, inv, content) =>
      implicit val o: Origin = inv.o
      Block(Seq(
        Exhale(dispatch(inv))(ParInvariantCannotBeExhaled(parInv)),
        dispatch(content),
        Inhale(dispatch(inv)),
      ))

    case parRegion @ ParStatement(impl) =>
      implicit val o: Origin = parRegion.o
      val (proc, args) = getRegionMethod(impl)
      emitChecks(impl)
      Eval(ProcedureInvocation[Post](proc.ref, args, Nil, Nil)(NoContext(ParPreconditionPreconditionFailed(impl))))

    case parBarrier @ ParBarrier(blockRef, invs, requires, ensures, content) =>
      implicit val o: Origin = parBarrier.o
      val block = parDecls(blockRef.decl)
      // TODO: it is a type-check error to have an invariant reference be out of scope in a barrier
      proveImplies(ParBarrierPostconditionFailed(parBarrier), dispatch(quantify(block, requires)), dispatch(quantify(block, ensures)), hint = dispatch(content))

      Block(Seq(
        Exhale(dispatch(requires))(ParBarrierExhaleFailed(parBarrier)),
        Inhale(dispatch(ensures)),
      ))

    case other => rewriteDefault(other)
  }
}

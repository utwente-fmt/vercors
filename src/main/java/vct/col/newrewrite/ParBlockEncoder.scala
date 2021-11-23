package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import Constant._
import vct.col.newrewrite.ParBlockEncoder.regionName
import vct.col.util.AstBuildHelpers._
import vct.col.newrewrite.util.{Extract, FreeVariables, Substitute}
import vct.col.origin._
import vct.col.rewrite.Rewriter

import scala.collection.mutable
case object ParBlockEncoder {
  def regionName(region: ParRegion): String = region match {
    case ParParallel(regions) => "par_$" + regions.map(regionName).mkString("_") + "$"
    case ParSequential(regions) => "seq_$" + regions.map(regionName).mkString("_") + "$"
    case block: ParBlock => block.o.preferredName
  }

  case class ParRegionImpl(region: ParRegion) extends Origin {
    override def messageInContext(message: String): String =
      region.o.messageInContext(message)

    override def preferredName: String = regionName(region)
  }

  case class ParBlockCheck(block: ParBlock) extends Origin {
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

  case class ParInvariantCannotBeExhaled(invariant: ParInvariant) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      invariant.blame.blame(ParInvariantNotEstablished(error.failure, invariant))
  }

  case class ParBarrierPostconditionFailed(barrier: ParBarrier) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      barrier.blame.blame(ParBarrierInconsistent(error.failure, barrier))
  }

  case class ParBarrierExhaleFailed(barrier: ParBarrier) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierNotEstablished(error.failure, barrier))
  }

  case class ParPreconditionPostconditionFailed(region: ParRegion) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParPreconditionPreconditionFailed(region: ParRegion) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParPostconditionPostconditionFailed(block: ParBlock) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      block.blame.blame(ParBlockPostconditionFailed(error.failure, block))
  }
}

case class ParBlockEncoder() extends Rewriter {
  import ParBlockEncoder._

  val invariants: ScopedStack[Expr] = ScopedStack()
  val parDecls: mutable.Map[ParBlockDecl, ParBlock] = mutable.Map()

  def quantify(block: ParBlock, expr: Expr)(implicit o: Origin): Expr = {
    val quantVars = block.iters.map(_.variable).map(v => v -> new Variable(v.t)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local(l.ref) -> Local(r.ref) }.toMap[Expr, Expr]).dispatch(expr)
    block.iters.foldLeft(body)((body, iter) => {
      val v = quantVars(iter.variable)
      Starall(Seq(v), Nil, (iter.from <= v.get && v.get < iter.to) ==> body)
    })
  }

  def proveImplies(blame: Blame[PostconditionFailed], antecedent: Expr, consequent: Expr, hint: Option[Statement] = None)(implicit origin: Origin): Unit = {
    val (Seq(req, ens), bindings) =
      Extract.extract(antecedent, consequent)

    procedure(
      blame = blame,
      requires = req,
      ensures = ens,
      args = bindings.keys.toSeq,
      body = Some(hint.getOrElse(Block(Nil))),
    ).declareDefault(this)
  }

  def requires(region: ParRegion)(implicit o: Origin): Expr = region match {
    case ParParallel(regions) => Star.fold(regions.map(requires))
    case ParSequential(regions) => regions.headOption.map(requires).getOrElse(tt)
    case block: ParBlock => quantify(block, block.requires)
  }

  def ensures(region: ParRegion)(implicit o: Origin): Expr = region match {
    case ParParallel(regions) => Star.fold(regions.map(ensures))
    case ParSequential(regions) => regions.headOption.map(ensures).getOrElse(tt)
    case block: ParBlock => quantify(block, block.ensures)
  }

  val regionAsMethod: mutable.Map[ParRegion, (Procedure, Seq[Expr])] = mutable.Map()

  def getRegionMethod(region: ParRegion): (Procedure, Seq[Expr]) = {
    implicit val o: Origin = ParImpl
    regionAsMethod.getOrElseUpdate(region, region match {
      case ParParallel(regions) =>
        val (Seq(req, ens), vars) = Extract.extract(requires(region), ensures(region))
        val result = procedure(
          blame = AbstractApplicable,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.toSeq)
      case ParSequential(regions) =>
        val (Seq(req, ens), vars) = Extract.extract(requires(region), ensures(region))

        val result = procedure(
          blame = AbstractApplicable,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.toSeq)
      case block: ParBlock =>
        val (Seq(req, ens), vars) = Extract.extract(requires(block), ensures(block))

        val result = procedure(
          blame = AbstractApplicable,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
        )(ParRegionImpl(region))
        result.declareDefault(this)
        (result, vars.values.toSeq)
    })
  }

  def emitChecks(region: ParRegion): Unit = region match {
    case ParParallel(regions) =>
      // The parallel composition of regions is automatically valid
      regions.foreach(emitChecks)
    case ParSequential(regions) =>
      // For sequential composition we verify that pairs of sequentially composed regions have a matching post- and precondition
      implicit val o: Origin = region.o
      regions.zip(regions.tail).foreach {
        case (left, right) =>
          proveImplies(ParPreconditionPostconditionFailed(right), ensures(left), requires(right))
      }
    case block @ ParBlock(decl, iters, req, ens, content) =>
      // For blocks we generate a separate check, by checking the contract for an indeterminate iteration
      implicit val o: Origin = region.o
      val extract = Extract()

      val ranges = iters.map {
        case IterVariable(v, from, to) => dispatch(from) <= v.get && v.get < dispatch(to)
      }.map(extract.extract)

      val requires = extract.extract(dispatch(req))
      val ensures = extract.extract(dispatch(ens))

      val body = extract.extract(dispatch(content))

      val vars = extract.finish()

      procedure(
        blame = ParPostconditionPostconditionFailed(block),
        args = vars.keys.toSeq,
        requires = Star.fold(ranges) &* requires,
        ensures = ensures,
        body = Some(body),
      )(ParBlockCheck(block))
  }


  override def dispatch(stat: Statement): Statement = stat match {
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
      Eval(ProcedureInvocation(proc.ref, args, Nil, Nil)(ParPreconditionPreconditionFailed(impl)))

    case parBarrier @ ParBarrier(blockRef, invs, requires, ensures, content) =>
      implicit val o: Origin = parBarrier.o
      val block = parDecls(blockRef.decl)
      // TODO: it is a type-check error to have an invariant reference be out of scope in a barrier
      proveImplies(ParBarrierPostconditionFailed(parBarrier), quantify(block, requires), quantify(block, ensures), hint = Some(content))

      Block(Seq(
        Exhale(dispatch(requires))(ParBarrierExhaleFailed(parBarrier)),
        Inhale(dispatch(ensures)),
      ))
  }
}

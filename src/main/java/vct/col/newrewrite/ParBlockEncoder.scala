package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import Constant._
import AstBuildHelpers._
import vct.col.newrewrite.util.{Extract, FreeVariables, Substitute}

import scala.collection.mutable
case class ParBlockEncoder() extends Rewriter {
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

  case class ParRegionExhaleFailed(region: ParRegion) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      region.blame.blame(ParRegionPreconditionFailed(error.failure, region))
  }

  case class ParRegionPreInconsistentPostconditionFailed(region: ParRegion) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      region.blame.blame(ParRegionPreconditionDoesNotImplyBlockPreconditions(error.failure, region))
  }

  case class ParRegionPostInconsistentPostconditionFailed(region: ParRegion) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      region.blame.blame(ParRegionPostconditionNotImpliedByBlockPostconditions(error.failure, region))
  }

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
      args = bindings.map(_._1),
      body = Some(hint.getOrElse(Block(Nil))),
    ).declareDefault(this)
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case parInv @ ParInvariant(decl, inv, content) =>
      implicit val o: Origin = inv.o
      Block(Seq(
        Exhale(dispatch(inv))(ParInvariantCannotBeExhaled(parInv)),
        dispatch(content),
        Inhale(dispatch(inv)),
      ))

    case parRegion @ ParRegion(specRequires, specEnsures, blocks) =>
      implicit val o: Origin = parRegion.o
      blocks.foreach(block => parDecls(block.decl) = block)

      val requires = specRequires match {
        case Constant(true) => Star.fold(blocks.map(block => quantify(block, block.requires)))
        case specified =>
          proveImplies(ParRegionPreInconsistentPostconditionFailed(parRegion),
            specified, Star.fold(blocks.map(block => quantify(block, block.requires))))
          specified
      }

      val ensures = specEnsures match {
        case Constant(true) => Star.fold(blocks.map(_.ensures))
        case specified =>
          proveImplies(ParRegionPostInconsistentPostconditionFailed(parRegion),
            Star.fold(blocks.map(block => quantify(block, block.ensures))), specified)
          specified
      }

      Block(Seq(
        Exhale(requires)(ParRegionExhaleFailed(parRegion)),
        Inhale(ensures),
      ))

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

package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import Constant._
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

  case class ParImplOrigin(preferredName: String) extends Origin {
    override def messageInContext(message: String): String = ???
  }
}

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

  def getRegionMethod(region: ParRegion)(implicit o: Origin): (Procedure, Seq[Expr]) =
    regionAsMethod.getOrElseUpdate(region, region match {
      case ParParallel(regions) =>
        val (Seq(req, ens), vars) = Extract.extract(requires(region), ensures(region))
        val result = procedure(
          blame = AbstractApplicable,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
        )
        result.declareDefault(this)
        (result, vars.values.toSeq)
      case ParSequential(regions) =>
        val preInvocations = regions.map(getRegionMethod).map {
          case (proc, args) => ProcedureInvocation(proc.ref, args, Nil, Nil)(???)
        }

        val (Seq(req, ens, invocations @ _*), vars) =
          Extract.extract(requires(region) +: ensures(region) +: preInvocations : _*)

        val result = procedure(
          blame = ???,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
          body = Some(Block(invocations.map(Eval(_)))),
        )
        result.declareDefault(this)
        (result, vars.values.toSeq)
      case block: ParBlock =>
        val (Seq(req, ens), vars) = Extract.extract(requires(block), ensures(block))

        val result = procedure(
          blame = AbstractApplicable,
          args = vars.keys.toSeq,
          requires = req,
          ensures = ens,
        )
        result.declareDefault(this)
        (result, vars.values.toSeq)
    })



  override def dispatch(stat: Statement): Statement = stat match {
    case parInv @ ParInvariant(decl, inv, content) =>
      implicit val o: Origin = inv.o
      Block(Seq(
        Exhale(dispatch(inv))(ParInvariantCannotBeExhaled(parInv)),
        dispatch(content),
        Inhale(dispatch(inv)),
      ))

    case parRegion @ ParStatement(impl) =>
      val (proc, args) = getRegionMethod(impl)(???)
      ???


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

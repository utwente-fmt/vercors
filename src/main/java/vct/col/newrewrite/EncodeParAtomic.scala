package vct.col.newrewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.newrewrite.ParBlockEncoder.EmptyHintCannotThrow
import vct.col.newrewrite.util.{Extract, Substitute}
import vct.col.origin.{Blame, CallableFailure, ContextEverywhereFailedInPost, ExceptionNotInSignals, ExhaleFailed, Origin, PanicBlame, ParBarrierInconsistent, ParBarrierMayNotThrow, ParBarrierNotEstablished, ParInvariantNotEstablished, ParInvariantNotMaintained, PostconditionFailed, SignalsFailed}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeParAtomic extends RewriterBuilder {
  case class ParInvariantCannotBeExhaled(invariant: ParInvariant[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      invariant.blame.blame(ParInvariantNotEstablished(error.failure, invariant))
  }

  case class ParAtomicCannotBeExhaled(atomic: ParAtomic[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      atomic.blame.blame(ParInvariantNotMaintained(error.failure, atomic))
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
}

case class EncodeParAtomic[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.newrewrite.EncodeParAtomic._

  val parDecls: mutable.Map[ParBlockDecl[Pre], ParBlock[Pre]] = mutable.Map()
  val invDecls: mutable.Map[ParInvariantDecl[Pre], ParInvariant[Pre]] = mutable.Map()

  def quantify(block: ParBlock[Pre], expr: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
    val quantVars = block.iters.map(_.variable).map(v => v -> new Variable[Pre](v.t)(v.o)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local[Pre](l.ref) -> Local[Pre](r.ref) }.toMap[Expr[Pre], Expr[Pre]]).dispatch(expr)
    block.iters.foldLeft(body)((body, iter) => {
      val v = quantVars(iter.variable)
      Starall(Seq(v), Nil, (iter.from <= v.get && v.get < iter.to) ==> body)
    })
  }

  def proveImplies(blame: Blame[CallableFailure], antecedent: Expr[Pre], consequent: Expr[Pre], hint: Statement[Pre])(implicit origin: Origin): Unit = {
    val extract = Extract[Pre]()
    val req = extract.extract(antecedent)
    val ens = extract.extract(consequent)
    val body = extract.extract(hint)
    val bindings = extract.finish().keys

    procedure[Post](
      blame = blame,
      requires = UnitAccountedPredicate(freshSuccessionScope { dispatch(req) }),
      ensures = UnitAccountedPredicate(freshSuccessionScope { dispatch(ens) }),
      args = collectInScope(variableScopes) { bindings.foreach(dispatch) },
      body = Some(dispatch(body)),
    ).declareDefault(this)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case parInv @ ParInvariant(decl, inv, content) =>
      implicit val o: Origin = inv.o
      invDecls(decl) = parInv
      decl.drop()
      Block(Seq(
        Exhale(freshSuccessionScope { dispatch(inv) })(ParInvariantCannotBeExhaled(parInv)),
        dispatch(content),
        Inhale(freshSuccessionScope { dispatch(inv) }),
      ))

    case atomic @ ParAtomic(inv, content) =>
      implicit val o: Origin = atomic.o
      Block(Seq(
        Block(inv.map(ref => Inhale[Post](freshSuccessionScope { dispatch(invDecls(ref.decl).inv) }))),
        dispatch(content),
        Block(inv.map(ref => Exhale[Post](freshSuccessionScope { dispatch(invDecls(ref.decl).inv) })(ParAtomicCannotBeExhaled(atomic)))),
      ))

    case parBarrier @ ParBarrier(blockRef, invs, requires, ensures, content) =>
      implicit val o: Origin = parBarrier.o
      val block = parDecls(blockRef.decl)
      // TODO: it is a type-check error to have an invariant reference be out of scope in a barrier
      proveImplies(
        ParBarrierPostconditionFailed(parBarrier),
        quantify(block, requires),
        quantify(block, ensures),
        hint = content,
      )

      Block(Seq(
        Exhale(dispatch(requires))(ParBarrierExhaleFailed(parBarrier)),
        Inhale(dispatch(ensures)),
      ))

    case other => rewriteDefault(other)
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Rewritten[Pre]] = parRegion match {
    case block: ParBlock[Pre] =>
      parDecls(block.decl) = block
      rewriteDefault(block)

    case other => rewriteDefault(other)
  }
}

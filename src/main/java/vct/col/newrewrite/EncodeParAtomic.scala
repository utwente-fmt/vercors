package vct.col.newrewrite

import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.newrewrite.util.FreshSuccessionScope
import vct.col.origin.{Blame, ExhaleFailed, Origin, ParInvariantNotEstablished, ParInvariantNotMaintained}
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
}

case class EncodeParAtomic[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.newrewrite.EncodeParAtomic._

  val invDecls: mutable.Map[ParInvariantDecl[Pre], ParInvariant[Pre]] = mutable.Map()

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case parInv @ ParInvariant(decl, inv, content) =>
      implicit val o: Origin = inv.o
      invDecls(decl) = parInv
      decl.drop()
      Block(Seq(
        Exhale(FreshSuccessionScope(this).dispatch(inv))(ParInvariantCannotBeExhaled(parInv)),
        dispatch(content),
        Inhale(FreshSuccessionScope(this).dispatch(inv)),
      ))

    case atomic @ ParAtomic(inv, content) =>
      implicit val o: Origin = atomic.o
      Block(Seq(
        Block(inv.map(ref => Inhale[Post](FreshSuccessionScope(this).dispatch(invDecls(ref.decl).inv)))),
        dispatch(content),
        Block(inv.map(ref => Exhale[Post](FreshSuccessionScope(this).dispatch(invDecls(ref.decl).inv))(ParAtomicCannotBeExhaled(atomic)))),
      ))

    case other => rewriteDefault(other)
  }
}

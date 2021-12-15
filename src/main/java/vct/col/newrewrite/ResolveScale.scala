package vct.col.newrewrite

import vct.col.ast.{Expr, InstancePredicateApply, PredicateApply, Scale}
import vct.col.newrewrite.ResolveScale.WrongScale
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.result.VerificationResult.UserError

case object ResolveScale extends RewriterBuilder {
  case class WrongScale(scale: Scale[_]) extends UserError {
    override def code: String = "wrongScale"
    override def text: String =
      scale.o.messageInContext("Permission scaling may only be applied to predicate applications.")
  }
}

case class ResolveScale[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val o: Origin = e.o
    e match {
      case Scale(scale, apply: PredicateApply[Pre]) =>
        apply.rewrite(perm = dispatch(scale) * dispatch(apply.perm))
      case Scale(scale, apply: InstancePredicateApply[Pre]) =>
        apply.rewrite(perm = dispatch(scale) * dispatch(apply.perm))
      case other: Scale[Pre] =>
        throw WrongScale(other)
      case other => rewriteDefault(other)
    }
  }
}

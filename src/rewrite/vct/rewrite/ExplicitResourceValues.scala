package vct.rewrite

import vct.col.ast.{
  CoerceResourceResourceVal,
  CoerceResourceValResource,
  Coercion,
  Expr,
  ResourceOfResourceValue,
  ResourceValue,
}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.col.typerules.CoercingRewriter

case object ExplicitResourceValues extends RewriterBuilder {
  override def key: String = "resourceValues1"
  override def desc: String =
    "Encode resoure value <-> resource conversion explicitly in the AST for later accounting."
}

case class ExplicitResourceValues[Pre <: Generation]()
    extends CoercingRewriter[Pre] {
  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceResourceResourceVal() => ResourceValue(e)
      case CoerceResourceValResource() => ResourceOfResourceValue(e)
      case other => super.applyCoercion(e, other)
    }
}

package vct.col.newrewrite

import vct.col.ast._
import vct.col.coerce.CoercingRewriter
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, RewriterBuilder}

case object SilverIntRatCoercion extends RewriterBuilder {
  override def key: String = "silverIntRat"
  override def desc: String = "Introduce an explicit node to convert integers to rationals for Silver."
}

case class SilverIntRatCoercion[Pre <: Generation]() extends CoercingRewriter[Pre] {
  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceIntRat() =>
      SilverIntToRat(e)
    case other => super.applyCoercion(e, other)
  }
}

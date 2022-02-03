package vct.col.newrewrite

import vct.col.ast._
import vct.col.coerce.{CoercingRewriter, Coercion}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

case object SilverIntRatCoercion extends RewriterBuilder

case class SilverIntRatCoercion[Pre <: Generation]() extends CoercingRewriter[Pre] {
  override def applyCoercion(e: Expr[Pre], coercion: Coercion[Pre])(implicit o: Origin): Expr[Pre] = coercion match {
    case Coercion.IntRat() =>
      SilverIntToRat(e)
    case other => super.applyCoercion(e, other)
  }
}

package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.Types

case object PinCollectionTypes extends RewriterBuilder

case class PinCollectionTypes[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case UntypedLiteralSeq(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralSeq[Post](dispatch(t), values.map(dispatch))(e.o)
    case UntypedLiteralSet(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralSet[Post](dispatch(t), values.map(dispatch))(e.o)
    case UntypedLiteralBag(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralBag[Post](dispatch(t), values.map(dispatch))(e.o)
    case other => rewriteDefault(other)
  }
}

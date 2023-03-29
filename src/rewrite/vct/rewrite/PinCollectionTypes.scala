package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.Types

case object PinCollectionTypes extends RewriterBuilder {
  override def key: String = "pinCollectionType"
  override def desc: String = "Derive and specify the element type of collections that have an inferred type."
}

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
    case OptNone() => OptNoneTyped[Post](TNothing()(e.o))(e.o)
    case OptSome(e) => OptSomeTyped[Post](dispatch(e.t), dispatch(e))(e.o)
    case other => rewriteDefault(other)
  }
}

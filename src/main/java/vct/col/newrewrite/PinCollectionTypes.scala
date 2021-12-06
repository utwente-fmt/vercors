package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.Rewriter
import vct.col.util.Types

case class PinCollectionTypes() extends Rewriter {
  override def dispatch(e: Expr): Expr = e match {
    case UntypedLiteralSeq(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralSeq(t, values.map(dispatch))(e.o)
    case UntypedLiteralSet(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralSet(t, values.map(dispatch))(e.o)
    case UntypedLiteralBag(values) =>
      val t = Types.leastCommonSuperType(values.map(_.t))
      LiteralBag(t, values.map(dispatch))(e.o)
    case other => rewriteDefault(other)
  }
}

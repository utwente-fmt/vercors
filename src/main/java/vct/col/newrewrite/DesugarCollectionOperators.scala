package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter

case class DesugarCollectionOperators() extends Rewriter {
  override def dispatch(e: Expr): Expr = {
    implicit val o: Origin = e.o
    e match {
      case Cons(head, tail) => Concat(LiteralSeq(head.t, Seq(dispatch(head))), dispatch(tail))
      case head @ Head(xs) => SeqSubscript(dispatch(xs), const(0))(head.blame)
      case Tail(xs) => Drop(dispatch(xs), const(1))
      case Slice(xs, from, to) => Drop(Take(dispatch(xs), dispatch(to)), dispatch(from))
      case RemoveAt(xsPre, iPre) =>
        val xs = dispatch(xsPre)
        val i = dispatch(iPre)
        Concat(
          Take(xs, i),
          Drop(xs, i + const(1))
        )
      case Empty(xs) => Size(dispatch(xs)) === const(0)
      case SubSetEq(xsPre, ysPre) =>
        val (xs, ys) = (dispatch(xsPre), dispatch(ysPre))
        SubSet(xs, ys) || xs === ys
      case other => rewriteDefault(other)
    }
  }
}

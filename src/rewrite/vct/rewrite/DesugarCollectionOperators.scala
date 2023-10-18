package vct.col.rewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object DesugarCollectionOperators extends RewriterBuilder {
  override def key: String = "desugarCollections"
  override def desc: String =
    "Desugar various operators related to collections."
}

case class DesugarCollectionOperators[Pre <: Generation]()
    extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case Cons(head, tail) =>
        Concat(
          LiteralSeq(dispatch(head.t), Seq(dispatch(head))),
          dispatch(tail),
        )
      case head @ Head(xs) => SeqSubscript(dispatch(xs), const(0))(head.blame)
      case Tail(xs) => Drop(dispatch(xs), const(1))
      case Slice(xs, from, to) =>
        Drop(Take(dispatch(xs), dispatch(to)), dispatch(from))
      case RemoveAt(xsPre, iPre) =>
        val xs = dispatch(xsPre)
        val i = dispatch(iPre)
        Concat(Take(xs, i), Drop(xs, i + const(1)))
      case Empty(xs) => Size(dispatch(xs)) === const(0)
      case SubSet(xsPre, ysPre) =>
        val (xs, ys) = (dispatch(xsPre), dispatch(ysPre))
        SubSetEq(xs, ys) && (Size(xs) !== Size(ys))
      case SubBag(xsPre, ysPre) =>
        val (xs, ys) = (dispatch(xsPre), dispatch(ysPre))
        SubBagEq(xs, ys) && (Size(xs) !== Size(ys))
      case other => rewriteDefault(other)
    }
  }
}

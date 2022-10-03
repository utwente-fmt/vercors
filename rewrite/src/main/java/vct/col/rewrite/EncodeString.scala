package vct.col.newrewrite

import vct.col.ast
import vct.col.ast.{Expr, LiteralSeq, StringLiteral, TInt, TSeq, TString, Type}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._

case object EncodeString extends RewriterBuilder {
  override def key: String = "encodeString"

  override def desc: String = "Encodes spec string using seq<int>."

}

case class EncodeString[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case StringLiteral(data) => LiteralSeq[Post](TInt(), data.map((c: Char) => const(c.toInt)(expr.o)))(expr.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TString() => TSeq[Post](TInt[Post]()(t.o))(t.o)
    case t => rewriteDefault(t)
  }
}
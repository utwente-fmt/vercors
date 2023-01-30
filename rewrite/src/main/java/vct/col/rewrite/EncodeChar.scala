package vct.col.rewrite

import vct.col.ast.{CharLiteral, Expr, TChar, TInt, Type}
import vct.col.util.AstBuildHelpers.const

case object EncodeChar extends RewriterBuilder {
  override def key: String = "encodeChar"
  override def desc: String = "Encodes char (unicode code points) using int."
}

case class EncodeChar[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case CharLiteral(data) =>
      assert(Character.charCount(data.codePointAt(0)) == data.length)
      const(data.codePointAt(0))(expr.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TChar() => TInt[Post]()(t.o)
    case t => rewriteDefault(t)
  }
}

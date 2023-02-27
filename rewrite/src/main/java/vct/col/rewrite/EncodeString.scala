package vct.col.rewrite

import vct.col.ast
import vct.col.ast.{CharValue, Concat, Expr, LiteralSeq, StringConcat, StringValue, TChar, TInt, TSeq, TString, Type}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.EncodeString.toCodepoints
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object EncodeString extends RewriterBuilder {
  override def key: String = "encodeString"
  override def desc: String = "Encodes spec string using seq<char>."

  def toCodepoints(str: String): Seq[Int] = {
    val all = ArrayBuffer[Int]()
    var offset = 0
    while (offset < str.length) {
      val codepoint = str.codePointAt(offset)
      all.addOne(codepoint)
      offset += Character.charCount(codepoint)
    }
    all.toSeq
  }
}

case class EncodeString[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case StringValue(data) => LiteralSeq[Post](TChar(), toCodepoints(data).map(CharValue(_)(expr.o)))(expr.o)
    case StringConcat(l, r) => Concat(dispatch(l), dispatch(r))(expr.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TString() => TSeq[Post](TChar[Post]()(t.o))(t.o)
    case t => rewriteDefault(t)
  }
}
package vct.col.rewrite

import vct.col.ast
import vct.col.ast.{CharLiteral, Expr, LiteralSeq, StringLiteral, TChar, TInt, TSeq, TString, Type}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.EncodeString.toCodepoints
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case object EncodeString extends RewriterBuilder {
  override def key: String = "encodeString"
  override def desc: String = "Encodes spec string using seq<char>."

  def toCodepoints(str: String): Seq[String] = {
    val all = ArrayBuffer[String]()
    var offset = 0
    while (offset < str.length) {
      val codepoint = str.codePointAt(offset)
      all.addOne(Character.toString(codepoint))
      offset += Character.charCount(codepoint)
    }
    all.toSeq
  }
}

case class EncodeString[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case StringLiteral(data) => LiteralSeq[Post](TChar(), toCodepoints(data).map(CharLiteral(_)(expr.o)))(expr.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TString() => TSeq[Post](TChar[Post]()(t.o))(t.o)
    case t => rewriteDefault(t)
  }
}
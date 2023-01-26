package vct.col.rewrite

import vct.col.ast.{Expr, Function, InternedString, Program, StringClassConcat}
import vct.col.origin.PanicBlame
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable

case object EncodeJavaLangString extends RewriterBuilder {
  override def key: String = "encodeJavaLangString"

  override def desc: String = "Encodes java.lang.String using interning functions and the builtin string type."
}

case class EncodeJavaLangString[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case i @ InternedString(data, interner) =>
      functionInvocation[Post](PanicBlame("Interning a java string cannot fail"), succ(interner.decl), Seq(dispatch(data)))(i.o)
    case c @ StringClassConcat(left, right, _, Ref(concatImpl)) =>
      functionInvocation[Post](PanicBlame("Concatenating java strings cannot fail"), succ(concatImpl), Seq(dispatch(left), dispatch(right)))(c.o)
    case other => rewriteDefault(other)
  }
}

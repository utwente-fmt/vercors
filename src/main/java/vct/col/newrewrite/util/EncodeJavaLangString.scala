package vct.col.newrewrite.util

import vct.col.ast.{Expr, Function, InternedString, JavaStringConcat, JavaStringConcatOperator, Program}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{BlameUnreachable, PanicBlame}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.Unreachable

case object EncodeJavaLangString extends RewriterBuilder {
  override def key: String = "encodeJavaLangString"

  override def desc: String = "Encodes java.lang.String using interning functions and the builtin string type."

}

case class EncodeJavaLangString[Pre <: Generation]() extends Rewriter[Pre] {

  var program: Program[Pre] = null
  lazy val concatImpl: Function[Pre] = findConcatImpl()
  def findConcatImpl(): Function[Pre] = {
    program.transSubnodes.foreach {
      case function: Function[Pre] if function.isPin(JavaStringConcatOperator()) =>
        return function
      case _ =>
    }

    throw Unreachable("???") // TODO (RR): Better error reporting
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program

    super.dispatch(program)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case i @ InternedString(data, interner) =>
      functionInvocation[Post](PanicBlame("Interning a java string cannot fail"), succ(interner.decl), Seq(dispatch(data)))(i.o)
    case c @ JavaStringConcat(left, right) =>
      functionInvocation[Post](PanicBlame("Concatenating java strings cannot fail"), succ(concatImpl), Seq(dispatch(left), dispatch(right)))(c.o)
    case other => rewriteDefault(other)
  }
}

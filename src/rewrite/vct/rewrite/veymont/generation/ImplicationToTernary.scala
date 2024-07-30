package vct.rewrite.veymont.generation

import vct.col.ast.{Expr, Implies, Select}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

object EncodeBooleanImplication extends RewriterBuilder {
  override def key: String = "encodeBooleanImplication"
  override def desc: String =
    "Rewrites implications `p ==> q` to `p ? q : true`."
}

case class EncodeBooleanImplication[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case Implies(p, q) => Select(dispatch(p), dispatch(q), tt)(expr.o)
      case _ => expr.rewriteDefault()
    }
}

package vct.col.newrewrite

import vct.col.ast.{Expr, InlinePattern}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case InlinePattern(inner) => dispatch(inner)
    case other => rewriteDefault(other)
  }
}

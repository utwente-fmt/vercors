package vct.col.newrewrite.util

import vct.col.ast.{Expr, Rewriter}

/**
 * Apply a substitution map to expressions
 */
case class Substitute(subs: Map[Expr, Expr]) extends Rewriter {
  override def dispatch(e: Expr): Expr = e match {
    case expr if subs.contains(expr) => subs(expr)
    case other => rewriteDefault(other)
  }
}

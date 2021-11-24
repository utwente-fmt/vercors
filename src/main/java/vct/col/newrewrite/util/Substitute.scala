package vct.col.newrewrite.util

import vct.col.ast.{Expr, TVar, Type}
import vct.col.rewrite.Rewriter

/**
 * Apply a substitution map to expressions
 */
case class Substitute(subs: Map[Expr, Expr], typeSubs: Map[TVar, Type] = Map.empty) extends Rewriter {
  override def dispatch(e: Expr): Expr = e match {
    case expr if subs.contains(expr) => subs(expr)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type): Type = t match {
    case v @ TVar(_) if typeSubs.contains(v) => typeSubs(v)
    case other => rewriteDefault(other)
  }
}

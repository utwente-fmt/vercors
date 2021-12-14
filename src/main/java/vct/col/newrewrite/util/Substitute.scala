package vct.col.newrewrite.util

import vct.col.ast.{Declaration, Expr, TVar, Type}
import vct.col.ref.Ref
import vct.col.rewrite.{NonLatchingRewriter, Rewriter}

import scala.reflect.ClassTag

/**
 * Apply a substitution map to expressions
 */
case class Substitute[G](subs: Map[Expr[G], Expr[G]], typeSubs: Map[TVar[G], Type[G]] = Map.empty[TVar[G], Type[G]]) extends NonLatchingRewriter[G, G] {
  override def succ[DPost <: Declaration[G]](decl: Declaration[G])(implicit tag: ClassTag[DPost]): Ref[G, DPost] =
    decl.asInstanceOf[DPost].ref

  override def dispatch(e: Expr[G]): Expr[G] = e match {
    case expr if subs.contains(expr) => subs(expr)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[G]): Type[G] = t match {
    case v @ TVar(_) if typeSubs.contains(v) => typeSubs(v)
    case other => rewriteDefault(other)
  }
}

package vct.col.newrewrite.util

import vct.col.ast.{Declaration, Expr, TVar, Type, Variable}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{NonLatchingRewriter, Rewriter}

import scala.reflect.ClassTag

/**
 * Apply a substitution map to expressions
 */
case class Substitute[G](subs: Map[Expr[G], Expr[G]],
                         typeSubs: Map[TVar[G], Type[G]] = Map.empty[TVar[G], Type[G]],
                         bindingSubs: Map[Variable[G], Variable[G]] = Map.empty[Variable[G], Variable[G]])
  extends NonLatchingRewriter[G, G] {

  override def succ[DPost <: Declaration[G]](decl: Declaration[G])(implicit tag: ClassTag[DPost]): Ref[G, DPost] =
    new LazyRef[G, DPost](successionMap.get(decl).getOrElse(decl))

  override def dispatch(e: Expr[G]): Expr[G] = e match {
    case expr if subs.contains(expr) => dispatch(subs(expr))
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[G]): Type[G] = t match {
    case v @ TVar(_) if typeSubs.contains(v) => dispatch(typeSubs(v))
    case other => rewriteDefault(other)
  }

  override def dispatch(v: Declaration[G]): Unit = v match {
    case decl: Variable[G] if bindingSubs.contains(decl) => dispatch(bindingSubs(decl))
    case other => rewriteDefault(other)
  }
}

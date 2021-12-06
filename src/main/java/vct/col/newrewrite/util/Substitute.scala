package vct.col.newrewrite.util

import vct.col.ast.{Declaration, Expr, TVar, Type}
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter

import scala.reflect.ClassTag

/**
 * Apply a substitution map to expressions
 */
case class Substitute(subs: Map[Expr, Expr], typeSubs: Map[TVar, Type] = Map.empty) extends Rewriter {
  override def succ[T <: Declaration](decl: Declaration)(implicit tag: ClassTag[T]): Ref[T] =
    decl.asInstanceOf[T].ref

  override def dispatch(e: Expr): Expr = e match {
    case expr if subs.contains(expr) => subs(expr)
    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type): Type = t match {
    case v @ TVar(_) if typeSubs.contains(v) => typeSubs(v)
    case other => rewriteDefault(other)
  }
}

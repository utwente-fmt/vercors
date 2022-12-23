package vct.col.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.NonLatchingRewriter

/**
 * Apply a substitution map to expressions
 */
case class Substitute[G](subs: Map[Expr[G], Expr[G]],
                         typeSubs: Map[TVar[G], Type[G]] = Map.empty[TVar[G], Type[G]],
                         bindingSubs: Map[Variable[G], Variable[G]] = Map.empty[Variable[G], Variable[G]],
                         originTrafo: Origin => Origin = identity)
  extends NonLatchingRewriter[G, G] {

  case class SuccOrIdentity() extends SuccessorsProviderTrafo[G, G](allScopes.freeze) {
    override def postTransform[T <: Declaration[G]](pre: Declaration[G], post: Option[T]): Option[T] =
      Some(post.getOrElse(pre.asInstanceOf[T]))
  }

  override def succProvider: SuccessorsProvider[G, G] = SuccOrIdentity()

  override def dispatch(o: Origin): Origin = originTrafo(o)

  override def dispatch(e: Expr[G]): Expr[G] = e match {
    case expr if subs.contains(expr) => subs(expr)
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

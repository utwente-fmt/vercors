package vct.rewrite.pallas

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

/** When the wrapper-functions of the Pallas contracts are inlined,
  * let-expressions are created which contain Perm(...) on the left-hand-side.
  * These are however not allowed in Viper. Hence, this pass inlines these
  * let-expressions (transitively). (Adaptation of InlineTrivialLets)
  */

object InlinePallasPermLets extends RewriterBuilder {
  override def key: String = "inlinePallasPermLets"
  override def desc: String =
    "Inlines let-expressions that contain Perm(...) on the lhs."
}

case class InlinePallasPermLets[Pre <: Generation]() extends Rewriter[Pre] {

  private val inPallasFunc: ScopedStack[Boolean] = ScopedStack()

  private val substitutions: ScopedStack[Map[Variable[Pre], Expr[Post]]] =
    ScopedStack()
  private def substitution: Map[Variable[Pre], Expr[Post]] =
    substitutions.topOption.getOrElse(Map.empty)

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] =>
        inPallasFunc.having(p.pallasFunction) { rewriteDefault(decl) }
      case _ => rewriteDefault(decl)
    }
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = {

    // Only inline things when we are in a Pallas-function
    if (inPallasFunc.isEmpty || inPallasFunc.top == false) {
      return expr.rewriteDefault()
    }

    expr match {
      case Let(v, e, inner) =>
        // Inline Let expressions:
        substitutions.having(substitution.updated(v, dispatch(e))) {
          dispatch(inner)
        }
      case Local(Ref(v)) if substitution.contains(v) => substitution(v)
      case _ => expr.rewriteDefault()
    }
  }

}

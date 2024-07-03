package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

object DropChorExpr extends RewriterBuilder {
  override def key: String = "dropChorExpr"

  override def desc: String = "Replaces \\chor expressions with `true`"
}

case class DropChorExpr[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case ChorExpr(_) => tt[Post]
      case _ => expr.rewriteDefault()
    }
}

package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.check.SeqProgParticipant
import vct.col.origin.{Blame, Origin, SeqBranchFailure}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

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

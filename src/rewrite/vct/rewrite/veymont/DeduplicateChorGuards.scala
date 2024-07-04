package vct.rewrite.veymont

import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

object DeduplicateChorGuards extends RewriterBuilder {
  override def key: String = "deduplicateSeqGuards"
  override def desc: String =
    "Deduplicates SeqGuard nodes with syntactically identical endpoints"
}

case class DeduplicateChorGuards[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) { super.dispatch(chor) }
      case _ => super.dispatch(decl)
    }

  override def dispatch(statement: Statement[Pre]): Statement[Post] =
    statement match {
      case InChor(_, c @ ChorStatement(branch: Branch[Pre])) =>
        c.rewrite(inner = branch.rewrite(branches = branch.branches.map {
          case (cond, stmt) => (dedup(cond), stmt.rewriteDefault())
        }))

      case InChor(_, c @ ChorStatement(loop: Loop[Pre])) =>
        c.rewrite(inner = loop.rewrite(cond = dedup(loop.cond)))

      case _ => rewriteDefault(statement)
    }

  def dedup(expr: Expr[Pre]): Expr[Post] = {
    implicit val o = expr.o
    val m: mutable.LinkedHashMap[Endpoint[Pre], Seq[Expr[Pre]]] = mutable
      .LinkedHashMap()
    ???
    // TODO: Optimize this for cases where it doesn't overlap so the origin thing doesn't even matter
    unfoldStar(expr).foreach {
      case EndpointExpr(Ref(endpoint), expr) =>
        m.updateWith(endpoint)(exprs => Some(exprs.getOrElse(Seq()) :+ expr))
      case _ => assert(false)
    }
    foldAnd(m.iterator.map { case (endpoint, parts) =>
      EndpointExpr[Post](succ(endpoint), foldAnd(parts.map(dispatch)))
    }.toSeq)
  }
}

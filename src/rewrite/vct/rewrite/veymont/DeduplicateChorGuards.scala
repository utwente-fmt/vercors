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
  override def veymontDispatch(p: Program[Pre]): Program[Post] =
    super.dispatch(p)

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

      case _ => statement.rewriteDefault()
    }

  def dedup(expr: Expr[Pre]): Expr[Post] = {
    implicit val o = expr.o
    val m: mutable.LinkedHashMap[Endpoint[Pre], Seq[Expr[Pre]]] = mutable
      .LinkedHashMap()
    unfoldStar(expr).foreach {
      case EndpointExpr(Ref(endpoint), expr) =>
        m.updateWith(endpoint)(exprs => Some(exprs.toSeq.flatten :+ expr))
      case _ =>
    }
    foldAnd(m.iterator.map {
      case (endpoint, parts) if parts.size > 1 =>
        // It's unclear how to properly combine the origins of the expressions here
        EndpointExpr[Post](succ(endpoint), foldAnd(parts.map(dispatch)))
      case (endpoint, parts) if parts.size == 1 =>
        EndpointExpr[Post](succ(endpoint), dispatch(parts.head))(parts.head.o)
    }.toSeq)
  }
}

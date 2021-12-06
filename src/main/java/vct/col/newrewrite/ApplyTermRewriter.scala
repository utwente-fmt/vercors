package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.rewrite.Rewriter
import vct.result.VerificationResult.UserError

import scala.annotation.tailrec
import scala.collection.mutable

case class ApplyTermRewriter(ruleNodes: Seq[SimplificationRule]) extends Rewriter {
  case class MalformedSimplificationRule(body: Expr) extends UserError {
    override def code: String = "malformedSimpRule"
    override def text: String =
      body.o.messageInContext(
        "The body of a simplification rule must be any number of nested \\forall predicates, " +
          "of which the body is an equality.")
  }

  val rules: Seq[(Seq[Variable], Expr, Expr)] = ruleNodes.map(node => consumeForalls(node.axiom)).map {
    case (free, body) => body match {
      case Eq(left, right) => (free, left, right)
      case other => throw MalformedSimplificationRule(other)
    }
  }

  def consumeForalls(node: Expr): (Seq[Variable], Expr) = node match {
    case Forall(bindings, _, body) =>
      val (innerBindings, innerBody) = consumeForalls(body)
      (bindings ++ innerBindings, innerBody)
    case other => (Nil, other)
  }

  def apply(rule: (Seq[Variable], Expr, Expr), subject: Expr): Option[Expr] = {
    val (free, left, right) = rule
    val inst = mutable.Map[Expr, Expr]()

    Comparator.compare(left, subject).foreach {
      case (local @ Local(v), value: Expr) if free.contains(v.decl) =>
        if(inst.getOrElseUpdate(local, value) != value) {
          return None
        }
      case (_, _) => return None
    }

    Some(Substitute(inst.toMap).dispatch(right))
  }

  @tailrec
  final def applyExhaustively(expr: Expr): Expr =
    FuncTools.firstOption(rules, apply(_, expr)) match {
      case Some(e) => applyExhaustively(e)
      case None => expr
    }

  override def dispatch(e: Expr): Expr =
    rewriteDefault(applyExhaustively(e))
}

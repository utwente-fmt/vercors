package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.UserError

import scala.annotation.tailrec
import scala.collection.mutable

case object ApplyTermRewriter {
  case class BuilderFor[Rule](ruleNodes: Seq[SimplificationRule[Rule]]) extends RewriterBuilder {
    override def apply[Pre <: Generation](): Rewriter[Pre] = ApplyTermRewriter(ruleNodes)
  }
}

case class ApplyTermRewriter[Rule, Pre <: Generation](ruleNodes: Seq[SimplificationRule[Rule]]) extends Rewriter[Pre] {
  case class MalformedSimplificationRule(body: Expr[_]) extends UserError {
    override def code: String = "malformedSimpRule"
    override def text: String =
      body.o.messageInContext(
        "The body of a simplification rule must be any number of nested \\forall predicates, " +
          "of which the body is an equality.")
  }

  val rules: Seq[(Seq[Variable[Rule]], Expr[Rule], Expr[Rule])] = ruleNodes.map(node => consumeForalls(node.axiom)).map {
    case (free, body) => body match {
      case Eq(left, right) => (free, left, right)
      case other => throw MalformedSimplificationRule(other)
    }
  }

  def consumeForalls(node: Expr[Rule]): (Seq[Variable[Rule]], Expr[Rule]) = node match {
    case Forall(bindings, _, body) =>
      val (innerBindings, innerBody) = consumeForalls(body)
      (bindings ++ innerBindings, innerBody)
    case other => (Nil, other)
  }

  def apply(rule: (Seq[Variable[Rule]], Expr[Rule], Expr[Rule]), subject: Expr[Pre]): Option[Expr[Pre]] = {
    val (free, left, right) = rule
    val inst = mutable.Map[Expr[Rule], Expr[Pre]]()

    Comparator.compare(left, subject).foreach {
      case (local @ Local(v), value: Expr[Pre]) if free.contains(v.decl) =>
        if(inst.getOrElseUpdate(local, value) != value) {
          return None
        }
      case (_, _) => return None
    }

    ???
    // Some(Substitute(inst.toMap).dispatch(right))
  }

  @tailrec
  final def applyExhaustively(expr: Expr[Pre]): Expr[Pre] =
    FuncTools.firstOption(rules, apply(_, expr)) match {
      case Some(e) => applyExhaustively(e)
      case None => expr
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    rewriteDefault(applyExhaustively(e))
}

package vct.col.ast.expr.binder

import hre.util.ScopedStack
import vct.col.ast.{
  Expr,
  InlinePattern,
  Let,
  Local,
  PossibleTrigger,
  TriggeredQuantifier,
  Variable,
}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{
  CheckContext,
  CheckError,
  DisallowedTriggerExpression,
  InvalidTriggerVars,
}
import vct.col.ref.Ref

import scala.annotation.tailrec
import scala.collection.mutable

trait TriggeredQuantifierImpl[G] extends NodeFamilyImpl[G] {
  this: TriggeredQuantifier[G] =>

  def triggers: Seq[Seq[Expr[G]]]
  def body: Expr[G]

  // This is not quite as good as the implementation in ExtractInlineQuantifierPatterns since it doesn't inline Lets
  def collectInlinePatterns: Seq[Seq[Expr[G]]] = {
    var depth = 0;
    body.flatCollect {
      case _: TriggeredQuantifier[G] =>
        depth += 1
        None
      case InlinePattern(e, group, inner) if inner - depth == 0 =>
        Seq((e, group))
    }.groupBy { case (_, group) => group }.values.map(_.map(_._1)).toSeq
  }

  @tailrec
  private def isPossibleTrigger(
      e: Expr[G],
      bindings: Map[Variable[G], Expr[G]],
  ): Boolean =
    e match {
      case pt: PossibleTrigger[G] => pt.isPossibleTrigger
      case Let(binding, value, main) =>
        isPossibleTrigger(main, bindings + (binding -> value))
      case Local(Ref(v)) if bindings.contains(v) =>
        isPossibleTrigger(bindings(v), bindings)
      case _ => false
    }

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    var result = super.check(context)

    // If variables haven't been resolved yet we cannot properly check if the binding match/ Luckily the checks after
    // LangSpecificToCol *also* get blamed on the user so it's fine to not do these check immediately.
    if (context.inResolution)
      return result

    val allTriggers = collectInlinePatterns ++ triggers

    // Each trigger set should mention all forall vars
    allTriggers.foreach { t =>
      val mentionedVars = t.flatMap(_.collect {
        case Local(Ref(v)) if bindings.contains(v) => v
      })
      val nonMentionedVars: Set[Variable[G]] = bindings.toSet -- mentionedVars
      if (nonMentionedVars.nonEmpty)
        result = result :+ InvalidTriggerVars(t, nonMentionedVars.toSet)
    }

    // Each trigger should be an expression that will eventually become one of Viper's AST nodes implementing PossibleTrigger
    result =
      result ++ allTriggers.flatMap[CheckError](
        _.filter(!isPossibleTrigger(_, Map.empty))
          .map(DisallowedTriggerExpression)
      )

    result
  }
}

package vct.col.ast.expr.binder

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  Expr,
  InlinePattern,
  Let,
  Local,
  Node,
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
  TriggerWithoutDependentVars,
}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.unfoldImplies

import scala.annotation.tailrec

trait TriggeredQuantifierImpl[G] extends NodeFamilyImpl[G] with LazyLogging {
  this: TriggeredQuantifier[G] =>

  def triggers: Seq[Seq[Expr[G]]]
  def body: Expr[G]

  private def collectLetBindings(e: Node[G]): Map[Variable[G], Expr[G]] =
    e match {
      case Let(binding, value, main) =>
        collectLetBindings(main) + (binding -> value)
      case _ =>
        e.subnodes.map(collectLetBindings(_))
          .fold[Map[Variable[G], Expr[G]]](Map.empty) { case (a, b) => a ++ b }
    }

  @tailrec
  private def isPossibleTrigger(
      e: Expr[G],
      bindings: Map[Variable[G], Expr[G]],
  ): Boolean =
    e match {
      case pt: PossibleTrigger[G] => pt.isPossibleTrigger
      case Local(Ref(v)) if bindings.contains(v) =>
        isPossibleTrigger(bindings(v), bindings)
      case _ => false
    }

  private def findMentionedVars(
      e: Node[G],
      bindings: Seq[Variable[G]],
      letBindings: Map[Variable[G], Expr[G]],
  ): Set[Variable[G]] =
    e match {
      case Local(Ref(v)) if bindings.contains(v) => Set(v)
      case Local(Ref(v)) if letBindings.contains(v) =>
        findMentionedVars(letBindings(v), bindings, letBindings)
      case _ =>
        e.subnodes.flatMap(findMentionedVars(_, bindings, letBindings)).toSet
    }

  def checkTriggers(triggerSets: Seq[Seq[Expr[G]]]): Seq[CheckError] = {
    val filteredTriggerSets = triggerSets.filter { t =>
      val present = t.forall((e: Node[G]) => body.exists(_ == e))
      // This is necessary since sometimes a pass (like simplify) removes terms from the body without removing them from
      // the triggers. Unfortunately this may also cause this check to ignore certain patterns that would otherwise be
      // caught as invalid. Therefore, it is possible that we get a crash at the end because we fail consistency checks.
      // This also happens quite a lot when the pattern is the Location inside a Perm/Value. However since that is a
      // restricted location anyway it should not be possible to get an invalid trigger in there.
      if (!present) {
        logger.debug(
          s"Ignoring trigger set: $t because it does not appear in the body of: $this"
        )
      }
      present
    }
    if (filteredTriggerSets.isEmpty || filteredTriggerSets.forall(_.isEmpty))
      return Nil
    var result: Seq[CheckError] = Nil
    val letBindings = collectLetBindings(body)

    // TODO: This is not quite good enough probably since this doesn't strip scales like unfoldBody in
    //       SimplifyNestedQuantifiers does.
    val (_, inner) = unfoldImplies(body)
    val dependentVars = findMentionedVars(inner, bindings, letBindings)
    if (dependentVars.isEmpty) { return Seq(TriggerWithoutDependentVars(this)) }
    // Each trigger set should mention all forall vars
    filteredTriggerSets.foreach { t =>
      val mentionedVars = t.flatMap(findMentionedVars(_, bindings, letBindings))
      val nonMentionedVars: Set[Variable[G]] = dependentVars -- mentionedVars
      if (nonMentionedVars.nonEmpty)
        result = result :+ InvalidTriggerVars(t, nonMentionedVars.toSet)
    }

    // Each trigger should be an expression that will eventually become one of Viper's AST nodes implementing PossibleTrigger
    result ++ filteredTriggerSets.flatMap[CheckError](
      _.filter(!isPossibleTrigger(_, letBindings))
        .map(DisallowedTriggerExpression)
    )
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    val result = super.check(context)

    // If variables haven't been resolved yet we cannot properly check if the binding match/ Luckily the checks after
    // LangSpecificToCol *also* get blamed on the user so it's fine to not do these check immediately.
    if (context.inResolution)
      return result

    result ++ checkTriggers(triggers)
  }
}

package vct.col.rewrite

import vct.col.ast._
import hre.util.ScopedStack
import vct.col.check.CheckError
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

import scala.collection.mutable.ArrayBuffer

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String =
    "Rewrite inline quantifier patterns into triggers."

  private case class InvalidInlineTrigger(errors: Seq[CheckError])
      extends UserError {
    override def code: String =
      s"inlinedPatterns:${errors.map(_.subcode).mkString(",")}"
    override def text: String = errors.map(_.message(_.o)).mkString("\n")
  }
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]()
    extends Rewriter[Pre] {
  outer =>
  import ExtractInlineQuantifierPatterns._
  // PB: letBindingsHere are all let bindings valid at the site where the trigger is defined: they need to be inlined,
  // since the extraction of the trigger may cross the body of the let.
  case class Pattern(
      group: Int,
      pattern: Expr[Pre],
      letBindingsHere: Map[Variable[Pre], Expr[Pre]],
  ) {
    def make(): Expr[Post] = MakePattern(this).dispatch(pattern)
  }

  private type LetBinding = (Variable[Pre], Expr[Pre])

  private val patterns: ScopedStack[ArrayBuffer[Pattern]] = ScopedStack()
  private val letBindings: ScopedStack[ScopedStack[LetBinding]] = ScopedStack()

  private case class MakePattern(pattern: Pattern) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    override def dispatch(e: Expr[Pre]): Expr[Post] =
      e match {
        case InlinePattern(inner, _, _) => dispatch(inner)

        case Local(Ref(v)) if pattern.letBindingsHere.contains(v) =>
          dispatch(pattern.letBindingsHere(v))

        case e => e.rewriteDefault()
      }
  }

  override def dispatch(p: Program[Pre]): Program[Post] =
    letBindings.having(ScopedStack()) { p.rewrite() }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case InLinePatternLocation(loc, pat) =>
        dispatch(pat)
        dispatch(loc)
      case other => other.rewriteDefault()
    }

  private def getTriggers(f: TriggeredQuantifier[Pre]): Seq[Seq[Expr[Post]]] = {
    val (patternsHere, _) = patterns.collect {
      // We only want to inline lets that are defined inside the quantifier
      letBindings.having(ScopedStack()) { dispatch(f.body) }
    }
    val unsortedGroups = patternsHere.groupBy(_.group)
    val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
    val preTriggers = sortedGroups.map(_.map(_.pattern))
    val errors = f.checkTriggers(preTriggers)
    if (errors.nonEmpty) {
      // Thrown here such that the checks in between passes don't blame this pass for invalid triggers, this isn't perfect
      // because if a pass generates an InlinePattern this will blame the user instead of the pass
      throw InvalidInlineTrigger(errors)
    }
    sortedGroups.map(_.map(_.make()))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case Let(binding, value, _) =>
        letBindings.top.having(binding -> value) { e.rewriteDefault() }

      case i: InlinePattern[Pre] =>
        if (patterns.toSeq.isDefinedAt(i.parent)) {
          // We only inline let bindings defined inside the current quantifier
          patterns.toSeq(i.parent) +=
            Pattern(i.group, i.inner, letBindings.top.toSeq.toMap)
        }
        dispatch(i.inner)
      case q: TriggeredQuantifier[Pre] =>
        q.rewrite(triggers = q.triggers.map(_.map(dispatch)) ++ getTriggers(q))
      case other => other.rewriteDefault()
    }
}

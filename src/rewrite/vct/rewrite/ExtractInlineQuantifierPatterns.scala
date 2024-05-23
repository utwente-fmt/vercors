package vct.col.rewrite

import vct.col.ast._
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ref.Ref
import vct.col.rewrite.ExtractInlineQuantifierPatterns.NotAllowedInTrigger
import vct.result.VerificationError.UserError

import scala.collection.mutable.ArrayBuffer

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."

  case class NotAllowedInTrigger(e: Expr[_]) extends UserError {
    override def code: String = "notAllowedInTrigger"
    override def text: String = e.o.messageInContext("Arithmetic and logic operators are not allowed in triggers.")
  }
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] { outer =>
  // PB: letBindingsHere are all let bindings valid at the site where the trigger is defined: they need to be inlined,
  // since the extraction of the trigger may cross the body of the let.
  case class Pattern(group: Int, pattern: Expr[Pre], letBindingsHere: Map[Variable[Pre], Expr[Pre]]) {
    def make(): Expr[Post] = MakePattern(this).dispatch(pattern)
  }

  type LetBinding = (Variable[Pre], Expr[Pre])

  val patterns: ScopedStack[ArrayBuffer[Pattern]] = ScopedStack()
  val letBindings: ScopedStack[ScopedStack[LetBinding]] = ScopedStack()

  case class MakePattern(pattern: Pattern) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes
    val inScale: ScopedStack[Unit] = ScopedStack()

    override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
      // PB: don't add nodes here just to be conservative: in general all terms are allowable in patterns, *except*
      // that z3 disallows all Bool-related operators, and Viper additionally disallows all arithmetic operators. Any
      // other operators is necessarily encoded as an smt function (allowed), or banned due to being a side effect
      // (later dealt with rigorously).
      case e if inScale.nonEmpty => e.rewriteDefault()

      case And(_, _) | Or(_, _) | Implies(_, _) | Star(_, _) | Wand(_, _) | PolarityDependent(_, _) =>
        throw NotAllowedInTrigger(e)

      case _: Forall[Pre] | _: Starall[Pre] | _: Exists[Pre] =>
        throw NotAllowedInTrigger(e)

      case Eq(_, _) | Neq(_, _) | Less(_, _) | Greater(_, _) | LessEq(_, _) | GreaterEq(_, _) =>
        throw NotAllowedInTrigger(e)

      case Plus(_, _) | Minus(_, _) | Mult(_, _) | FloatDiv(_, _) | RatDiv(_, _) | FloorDiv(_, _) | Mod(_, _) =>
        throw NotAllowedInTrigger(e)

      case InlinePattern(inner, _, _) => dispatch(inner)

      case Local(Ref(v)) if pattern.letBindingsHere.contains(v) => dispatch(pattern.letBindingsHere(v))

      case p@PredicateApply(ref, args, perm) =>
        val newPerm = inScale.having(()){
          dispatch(perm)
        }
        PredicateApply(succ[Predicate[Post]](ref.decl), args.map(dispatch), newPerm)(e.o)

      case e => rewriteDefault(e)
    }
  }

  override def dispatch(p: Program[Pre]): Program[Post] =
    letBindings.having(ScopedStack()) { p.rewrite() }

  override def dispatch(loc: Location[Pre]): Location[Post] = loc match {
    case InLinePatternLocation(loc, pat) =>
      dispatch(pat)
      dispatch(loc)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Let(binding, value, _) =>
      letBindings.top.having(binding -> value) { rewriteDefault(e) }

    case i: InlinePattern[Pre] =>
      if(patterns.toSeq.isDefinedAt(i.parent)) {
        // We only inline let bindings defined inside the current quantifier
        patterns.toSeq(i.parent) += Pattern(i.group, i.inner, letBindings.top.toSeq.toMap)
      }
      dispatch(i.inner)

    case f: Forall[Pre] =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          // We only want to inline lets that are defined inside the quantifier
          letBindings.having(ScopedStack()) {
            dispatch(f.body)
          }
        }
        val unsortedGroups = patternsHere.groupBy(_.group)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_.make()))
        Forall(
          bindings = variables.collect { f.bindings.foreach(dispatch) }._1,
          triggers = f.triggers.map(_.map(dispatch)) ++ triggers,
          body = body
        )(f.o)
      }

    case f: Starall[Pre] =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          // We only want to inline lets that are defined inside the quantifier
          letBindings.having(ScopedStack()) {
            dispatch(f.body)
          }
        }
        val unsortedGroups = patternsHere.groupBy(_.group)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_.make()))
        Starall(
          bindings = variables.collect {
            f.bindings.foreach(dispatch)
          }._1,
          triggers = f.triggers.map(_.map(dispatch)) ++ triggers,
          body = body
        )(f.blame)(f.o)
      }

    case f: Exists[Pre] =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          // We only want to inline lets that are defined inside the quantifier
          letBindings.having(ScopedStack()) {
            dispatch(f.body)
          }
        }
        val unsortedGroups = patternsHere.groupBy(_.group)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_.make()))
        Exists(
          bindings = variables.collect {
            f.bindings.foreach(dispatch)
          }._1,
          triggers = f.triggers.map(_.map(dispatch)) ++ triggers,
          body = body
        )(f.o)
      }

    case other => rewriteDefault(other)
  }
}

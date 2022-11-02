package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import RewriteHelpers._
import hre.util.ScopedStack

import scala.collection.mutable.ArrayBuffer

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] {
  val patterns: ScopedStack[ArrayBuffer[(Int, Expr[Pre])]] = ScopedStack()

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case i: InlinePattern[Pre] =>
      if(patterns.toSeq.isDefinedAt(i.parent)) {
        patterns.toSeq(i.parent) += i.group -> i.inner
      }
      dispatch(i.inner)

    case f: Forall[Pre] =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          dispatch(f.body)
        }
        val unsortedGroups = patternsHere.groupBy(_._1)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_._2).map(dispatch))
        Forall(
          bindings = variables.collect { f.bindings.foreach(dispatch) }._1,
          triggers = f.triggers.map(_.map(dispatch)) ++ triggers,
          body = body
        )(f.o)
      }

    case f: Starall[Pre] =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          dispatch(f.body)
        }
        val unsortedGroups = patternsHere.groupBy(_._1)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_._2).map(dispatch))
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
          dispatch(f.body)
        }
        val unsortedGroups = patternsHere.groupBy(_._1)
        val sortedGroups = unsortedGroups.toSeq.sortBy(_._1).map(_._2)
        val triggers = sortedGroups.map(_.map(_._2).map(dispatch))
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

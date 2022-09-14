package vct.col.newrewrite

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
  val patterns: ScopedStack[ArrayBuffer[Expr[Pre]]] = ScopedStack()

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case i: InlinePattern[Pre] =>
      patterns.topOption.foreach { _ += i.inner }
      dispatch(i.inner)

    case f: Forall[Pre] if f.triggers.isEmpty =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          dispatch(f.body)
        }
        val triggers = if (patternsHere.isEmpty) Nil else Seq(patternsHere.map(dispatch))
        Forall(
          bindings = variables.collect { f.bindings.foreach(dispatch) }._1,
          triggers = triggers,
          body = body
        )(f.o)
      }

    case f: Starall[Pre] if f.triggers.isEmpty =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          dispatch(f.body)
        }
        val triggers = if (patternsHere.isEmpty) Nil else Seq(patternsHere.map(dispatch))
        Starall(
          bindings = variables.collect {
            f.bindings.foreach(dispatch)
          }._1,
          triggers = triggers,
          body = body
        )(f.blame)(f.o)
      }

    case f: Exists[Pre] if f.triggers.isEmpty =>
      variables.scope {
        val (patternsHere, body) = patterns.collect {
          dispatch(f.body)
        }
        val triggers = if (patternsHere.isEmpty) Nil else Seq(patternsHere.map(dispatch))
        Exists(
          bindings = variables.collect {
            f.bindings.foreach(dispatch)
          }._1,
          triggers = triggers,
          body = body
        )(f.o)
      }

    case other => rewriteDefault(other)
  }
}

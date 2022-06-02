package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable;

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] {
  val claimedPatterns: mutable.Set[InlinePattern[Pre]] = mutable.Set()

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case i: InlinePattern[Pre] => dispatch(i.inner)
    case f: Forall[Pre] =>
      // TODO (RR): This causes an error...
      if (f.triggers.nonEmpty) {
        rewriteDefault(f)
      } else {
        val body = dispatch(f.body)
        val triggersInBody: Seq[InlinePattern[Pre]] = f.body.transSubnodes.collect({ case it: InlinePattern[Pre] => it })
        val leftoverTriggers = triggersInBody.filter(!claimedPatterns.contains(_))
        val triggers = leftoverTriggers.map(t => Seq(dispatch(t)))
        claimedPatterns.addAll(leftoverTriggers)
        f.rewrite(body = body, triggers = triggers)
      }
    case other => rewriteDefault(other)
  }
}


/*
forall i: .... forall j: ... {: f(i) :} {: f(j) :}
===
forall i: .... forall j: ... {: f(i) :}
*/
package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import RewriteHelpers._

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] {
  def someTransSubnodes[G](x: Node[G], P: Node[G] => Boolean): LazyList[Node[G]] = {
    if (P(x)) {
      x #:: Subnodes.subnodes(x).to(LazyList).flatMap(someTransSubnodes(_, P))
    } else {
      LazyList()
    }
  }

  def notForall[G](x: Node[G]): Boolean = x match {
    case _: Forall[G] => false
    case _ => true
  }

  def refersVars[G](e: Expr[G]): LazyList[Variable[G]] =
    e.transSubnodes.collect({ case Local(r) => r.decl })

  def getInlineTriggers[G](bindings: Seq[Variable[G]], e: Expr[G]): Seq[InlinePattern[G]] =
    someTransSubnodes(e, notForall[G]).collect({ case it: InlinePattern[G] => it })
      // Only keep inline patterns that refer to at least _one_ quantified variable
      .filter(ipat => refersVars(ipat).exists(bindings.contains(_)))
      .toIndexedSeq

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case i: InlinePattern[Pre] => dispatch(i.inner)
    case f: Forall[Pre] =>
      if (f.triggers.nonEmpty) {
        rewriteDefault(f)
      } else {
        val triggers = getInlineTriggers(f.bindings, f.body).map(t => dispatch(t)) match {
          case Nil => Seq()
          case ts => Seq(ts)
        }
        f.rewrite(triggers = triggers)
      }
    case f: Starall[Pre] =>
      if (f.triggers.nonEmpty) {
        rewriteDefault(f)
      } else {
        val triggers = getInlineTriggers(f.bindings, f.body).map(t => dispatch(t)) match {
          case Nil => Seq()
          case ts => Seq(ts)
        }
        f.rewrite(triggers = triggers)
      }
    case other => rewriteDefault(other)
  }
}

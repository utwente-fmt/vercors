package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._

case object ExtractInlineQuantifierPatterns extends RewriterBuilder {
  override def key: String = "inlineQuantifierPattern"
  override def desc: String = "Rewrite inline quantifier patterns into triggers."
}

case class ExtractInlineQuantifierPatterns[Pre <: Generation]() extends Rewriter[Pre] {
  def someTransSubnodes[G](x: Node[G], P: Node[G] => Boolean): LazyList[Node[G]] = {
    val tail = Subnodes.subnodes(x).to(LazyList).filter(P).flatMap(someTransSubnodes(_, P))
    if (P(x)) {
      x #:: tail
    } else {
      tail
    }
  }

  def notForall[G](x: Node[G]): Boolean = x match {
    case f: Forall[G] => false
    case _ => true
  }

  def getInlineTriggers[G](e: Expr[G]): IndexedSeq[InlinePattern[G]] =
    someTransSubnodes(e, notForall[G]).collect({ case it: InlinePattern[G] => it }).toIndexedSeq

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case i: InlinePattern[Pre] => dispatch(i.inner)
    case f: Forall[Pre] =>
      if (f.triggers.nonEmpty) {
        rewriteDefault(f)
      } else {
        f.rewrite(triggers = getInlineTriggers(f.body).map(t => Seq(dispatch(t))))
      }
    case f: Starall[Pre] =>
      if (f.triggers.nonEmpty) {
        rewriteDefault(f)
      } else {
        f.rewrite(triggers = getInlineTriggers(f.body).map(t => Seq(dispatch(t))))
      }
    case other => rewriteDefault(other)
  }
}

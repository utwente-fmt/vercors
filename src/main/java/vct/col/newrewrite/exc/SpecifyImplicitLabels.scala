package vct.col.newrewrite.exc

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case class ImplicitLabelOrigin(inner: Origin) extends Origin {
  override def preferredName: String = "loop"
  override def context: String = inner.context
}

case object SpecifyImplicitLabels extends RewriterBuilder {
  override def key: String = "implicitLabels"
  override def desc: String = "Give loops and switches a label if it needs one for a break or continue statement."
}

case class SpecifyImplicitLabels[Pre <: Generation]() extends Rewriter[Pre] {
  val labelStack = new ScopedStack[LabelDecl[Post]]()

  def isBreakable(s: Statement[_]): Boolean = s match {
    case _: Loop[_] => true
    case _: Switch[_] => true
    case _ => false
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Label(decl, impl) if isBreakable(impl) =>
      val newLabel = decl.rewrite()
      succeed(decl, newLabel)
      val newImpl = labelStack.having(newLabel) {
        rewriteDefault(impl)
      }
      Label(newLabel, newImpl)(stat.o)
    case stat if isBreakable(stat) =>
      implicit val o: Origin = stat.o
      val labelDecl = new LabelDecl[Post]()(ImplicitLabelOrigin(o))
      labelStack.having(labelDecl) {
        Label(labelDecl, rewriteDefault(stat))
      }
    case c@Continue(None) =>
      c.rewrite(Some(labelStack.top.ref))
    case b@Break(None) =>
      b.rewrite(Some(labelStack.top.ref))

    case other => rewriteDefault(other)
  }
}

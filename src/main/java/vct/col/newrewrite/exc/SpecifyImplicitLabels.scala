package vct.col.newrewrite.exc

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter

case class ImplicitLabelOrigin(inner: Origin) extends Origin {
  override def preferredName: String = "implicitLabel"
  override def messageInContext(message: String): String = inner.messageInContext(message)
}

case class SpecifyImplicitLabels() extends Rewriter {
  val labelStack = new ScopedStack[LabelDecl]()

  def isBreakable(s: Statement) = s match {
    case _: Loop => true
    case _: Switch => true
    case _ => false
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case Label(decl, impl) if isBreakable(impl) =>
      val newLabel = decl.rewrite()
      val newImpl = labelStack.having(newLabel) {
        rewriteDefault(impl)
      }
      Label(newLabel, newImpl)(stat.o)
    case stat if isBreakable(stat) =>
      implicit val o: Origin = stat.o
      val labelDecl = new LabelDecl()(ImplicitLabelOrigin(o))
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

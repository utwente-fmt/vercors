package vct.col.newrewrite

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
    /*
    PB TODO: fix as with continuetobreak
    case block@Block(Seq(oldLabel@Label(_), s: Statement)) if isBreakable(s) =>
      val newLabel = oldLabel.rewrite()
      val newS = labelStack.having(newLabel.decl) {
        rewriteDefault(s)
      }
      block.rewrite(statements = Seq(newLabel, newS))
    case s: Statement if isBreakable(s) =>
      implicit val o = s.o
      val labelDecl = new LabelDecl()(ImplicitLabelOrigin(o))
      val labelStatement = Label(labelDecl)
      labelStack.having(labelDecl) {
        Block(Seq(labelStatement, rewriteDefault(s)))
      }
    case c@Continue(None) =>
      c.rewrite(Some(labelStack.top.ref))
    case b@Break(None) =>
      b.rewrite(Some(labelStack.top.ref))
     */
    case other => rewriteDefault(other)
  }
}

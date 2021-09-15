package vct.col.newrewrite

import vct.col.ast.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.ast._

import scala.collection.mutable

// We don't include the origin of the Continue statement here, because that doesn't make sense if there are
// multiple continue statements referring to the same label. In that case, we only refer to label origin, as
// that is what all continues have in common. This should be changed if it results in a bad user experience.
case class ContinueToBreakOrigin(labelDeclOrigin: Origin) extends Origin {
  override def preferredName: String = "continue" + labelDeclOrigin.preferredName.capitalize
  override def messageInContext(message: String): String = labelDeclOrigin.messageInContext(message)
}

case class ContinueToBreak() extends Rewriter {
  val loopLabelToInnerLabel = new mutable.HashMap[LabelDecl, LabelDecl]()

  override def dispatch(stat: Statement): Statement = stat match {
    case block@Block(Seq(l@Label(labelDecl), loop: Loop)) =>
      val rewrittenBody = dispatch(loop.body)

      // If the loop label appears in the mapping, it means it contains some continue that wants to break
      // from the inner label. We create a block wrapping the body, labeled with the inner label.
      val possiblyWrappedBody = loopLabelToInnerLabel.get(labelDecl) match {
        case Some(innerLabelDecl) =>
          implicit val o = innerLabelDecl.o
          Block(Seq(Label(innerLabelDecl), rewrittenBody))
        case None => rewrittenBody
      }

      block.rewrite(Seq(l.rewrite(), loop.rewrite(body = possiblyWrappedBody)))

    case c@Continue(Some(Ref(labelDecl: LabelDecl))) =>
      // Reuse an already created inner label or create one
      val innerLabelDecl = loopLabelToInnerLabel.getOrElseUpdate(
        labelDecl,
        new LabelDecl()(ContinueToBreakOrigin(labelDecl.o))
      )
      Break(Some(innerLabelDecl.ref))(c.o)

    case other => rewriteDefault(other)
  }
}

package vct.col.newrewrite.exc

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import scala.collection.mutable

// We don't include the origin of the Continue statement here, because that doesn't make sense if there are
// multiple continue statements referring to the same label. In that case, we only refer to label origin, as
// that is what all continues have in common. This should be changed if it results in a bad user experience.
case class ContinueToBreakOrigin(labelDeclOrigin: Origin) extends Origin {
  override def preferredName: String = "continue" + labelDeclOrigin.preferredName.capitalize
  override def shortPosition: String = "generated"
  override def context: String = labelDeclOrigin.context
  override def inlineContext: String = labelDeclOrigin.inlineContext
}

case object ContinueToBreak extends RewriterBuilder {
  override def key: String = "continueToBreak"
  override def desc: String = "Encode continue as a break statement."
}

case class ContinueToBreak[Pre <: Generation]() extends Rewriter[Pre] {
  val loopLabelToInnerLabel = new mutable.HashMap[LabelDecl[Pre], LabelDecl[Post]]()

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Label(labelDecl, loop: Loop[Pre]) =>
      val rewrittenBody = dispatch(loop.body)

      // If the loop label appears in the mapping, it means it contains some continue that wants to break
      // from the inner label. We create a block wrapping the body, labeled with the inner label.
      val possiblyWrappedBody = loopLabelToInnerLabel.get(labelDecl) match {
        case Some(innerLabelDecl) =>
          implicit val o: Origin = innerLabelDecl.o
          Label(innerLabelDecl, rewrittenBody)
        case None => rewrittenBody
      }

      Label(labelDecls.dispatch(labelDecl), loop.rewrite(body = possiblyWrappedBody))(stat.o)

    case c@Continue(Some(Ref(labelDecl))) =>
      // Reuse an already created inner label or create one
      val innerLabelDecl = loopLabelToInnerLabel.getOrElseUpdate(
        labelDecl,
        new LabelDecl()(ContinueToBreakOrigin(labelDecl.o))
      )
      Break(Some(innerLabelDecl.ref))(c.o)

    case other => rewriteDefault(other)
  }
}

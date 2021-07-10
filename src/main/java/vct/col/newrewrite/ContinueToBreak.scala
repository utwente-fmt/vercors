package vct.col.newrewrite

import vct.col.ast.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.ast._

import scala.collection.mutable

case class ContinueToBreak() extends Rewriter {
  val loopLabelToInnerLabel = new mutable.HashMap[LabelDecl, LabelDecl]()

  override def dispatch(stat: Statement): Statement = stat match {
    case block@Block(Seq(l@Label(labelDecl), loop: Loop)) =>
      val rewrittenBody = rewriteDefault(loop.body)

      // If the loop label appears in the mapping, it means it contains some continue that wants to break
      // from the inner label. We use create a block wrapping the body, labeled with the inner label.
      val possiblyWrappedBody = loopLabelToInnerLabel.get(labelDecl) match {
        case Some(innerLabelDecl) =>
          implicit val o = loop.o
          Block(Seq(Label(innerLabelDecl), rewrittenBody))
        case None => rewrittenBody
      }

      block.rewrite(Seq(rewriteDefault(l), loop.rewrite(body = possiblyWrappedBody)))

    case c@Continue(Some(Ref(labelDecl: LabelDecl))) =>
      // Reuse an already created inner label or create one
      val innerLabelDecl = loopLabelToInnerLabel.get(labelDecl) match {
        case Some(l) => l
        case None =>
          val innerLabelDecl = new LabelDecl()(SourceNameOrigin("continue", labelDecl.o))
          loopLabelToInnerLabel.put(labelDecl, innerLabelDecl)
          innerLabelDecl
      }

      Break(Some(innerLabelDecl.ref))(c.o)

    case other => rewriteDefault(other)
  }
}

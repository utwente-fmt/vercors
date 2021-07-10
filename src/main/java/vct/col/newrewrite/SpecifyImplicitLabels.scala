package vct.col.newrewrite

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.AstBuildHelpers._

import scala.collection.mutable

case class SpecifyImplicitLabels() extends Rewriter {
  val labelStack = new mutable.Stack[LabelDecl]()

  override def dispatch(stat: Statement): Statement = stat match {
    case block@Block(Seq(Label(labelDecl), _: Loop)) =>
      labelStack.push(labelDecl)
      val res = rewriteDefault(block)
      labelStack.pop()
      res
    case block@Block(Seq(Label(labelDecl), _: Switch)) =>
      labelStack.push(labelDecl)
      val res = rewriteDefault(block)
      labelStack.pop()
      res
    case loop: Loop =>
      implicit val o = loop.o
      val labelDecl = new LabelDecl()(SourceNameOrigin("loop", o))
      val labelStatement = Label(labelDecl)
      labelStack.push(labelDecl)
      val res = Block(Seq(labelStatement, rewriteDefault(loop)))
      labelStack.pop()
      res
    case switch: Switch =>
      implicit val o = switch.o
      val labelDecl = new LabelDecl()(SourceNameOrigin("switch", o))
      val labelStatement = Label(labelDecl)
      labelStack.push(labelDecl)
      val res = Block(Seq(labelStatement, rewriteDefault(switch)))
      labelStack.pop()
      res
    case c@Continue(None) =>
      c.rewrite(Some(labelStack.top.ref))
    case b@Break(None) =>
      b.rewrite(Some(labelStack.top.ref))
    case other => rewriteDefault(other)
  }
}

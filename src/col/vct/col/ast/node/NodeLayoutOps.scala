package vct.col.ast.node

import vct.col.ast.Node
import vct.col.print.{Ctx, Doc, Group, NodeDoc, Text}
import vct.col.ref.Ref
import vct.result.HasContext

trait NodeLayoutOps[G] {
  this: Node[G] =>

  def debugLayout(x: scala.Any)(implicit ctx: Ctx): Doc =
    x match {
      case n: Node[_] => n.show
      case r: Ref[_, _] => Text("Ref(") <> ctx.name(r) <> ")"
      case o: scala.Option[scala.Any] if o.isEmpty => Text("None")
      case o: scala.Option[scala.Any] =>
        Text("Some(") <> debugLayout(o.get) <> ")"
      case i: scala.Iterable[scala.Any] =>
        Group(
          Text(i.getClass.getSimpleName) <> "(" <>
            Doc.args(i.map(debugLayout).toSeq) <> ")"
        )
      case p: scala.Product =>
        Group(
          Text(p.getClass.getSimpleName) <> "(" <>
            Doc.args(p.productIterator.map(debugLayout).toSeq) <> ")"
        )
      case other => Text(other.toString)
    }

  final def show(implicit ctx: Ctx): Doc = NodeDoc(this, layout)

  override def toString: String = toStringWithContext(Ctx().namesIn(this))

  def toInlineString: String = {
    implicit val ctx = Ctx().namesIn(this).copy(width = Int.MaxValue)
    Group(show).toStringWithContext
  }

  def highlight(node: Node[_]): HasContext =
    new HasContext {
      def contextText: String = {
        implicit val ctx: Ctx = Ctx().namesIn(NodeLayoutOps.this)
        show.highlight(node).strip()
      }
    }
}

package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralSeq, TSeq, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.LiteralSeqOps

trait LiteralSeqImpl[G] extends LiteralSeqOps[G] {
  this: LiteralSeq[G] =>
  override def t: Type[G] = TSeq(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar =>
        if (values.isEmpty) { Group(Text("[]")) }
        else { Group(Text("[") <> Doc.args(values) <> Text(")")) }
      case _ =>
        Group(Text("seq<") <> element <> ">{" <> Doc.args(values) <> "}")
    }
  }
}

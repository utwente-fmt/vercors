package vct.col.ast.`type`

import vct.col.ast.{TSeq, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TSeqOps

trait TSeqImpl[G] extends TSeqOps[G] {
  this: TSeq[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      ctx.syntax match {
        case Ctx.Isar => open <> Doc.arg(element) <> close <+> Text("list")
        case _ => Text("seq") <> open <> Doc.arg(element) <> close
      }
    )
  }

  val subtypes: Seq[Type[G]] = Seq(element)
}

package vct.col.ast.`type`

import vct.col.ast.{TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TMapOps

trait TMapImpl[G] extends TMapOps[G] {
  this: TMap[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Group(ctx.syntax match {
      case Ctx.Isar =>
        open <> Doc.args(Seq(key, value)) <> close <+> Text("fmap")
      case _ => Text("maps") <> open <> Doc.args(Seq(key, value)) <> close
    })
  }

  val subtypes: Seq[Type[G]] = Seq(key, value)
}

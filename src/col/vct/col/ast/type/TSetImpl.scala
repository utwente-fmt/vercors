package vct.col.ast.`type`

import vct.col.ast.{TSet, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TSetOps

trait TSetImpl[G] extends TSetOps[G] {
  this: TSet[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("set") <> open <> Doc.arg(element) <> close)
  val subtypes: Seq[Type[G]] = Seq(element)
}

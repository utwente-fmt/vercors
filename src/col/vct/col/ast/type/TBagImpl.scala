package vct.col.ast.`type`

import vct.col.ast.{TBag, Type}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}
import vct.col.ast.ops.TBagOps

trait TBagImpl[G] extends TBagOps[G] {
  this: TBag[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("bag") <> open <> Doc.arg(element) <> close)
  val subtypes: Seq[Type[G]] = Seq(element)
}

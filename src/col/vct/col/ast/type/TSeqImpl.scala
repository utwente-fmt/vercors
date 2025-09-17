package vct.col.ast.`type`

import vct.col.ast.{TSeq, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TSeqOps
import vct.col.typerules.TypeSize

trait TSeqImpl[G] extends TSeqOps[G] {
  this: TSeq[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("seq") <> open <> Doc.arg(element) <> close)
  val subtypes: Seq[Type[G]] = Seq(element)
  // Temporary, it should be possible to mark fields as ghost fields which do not contribute to the size of a struct
  override def bits: TypeSize = TypeSize.Exact(8)
}

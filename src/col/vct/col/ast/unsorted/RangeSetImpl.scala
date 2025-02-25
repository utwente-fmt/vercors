package vct.col.ast.unsorted

import vct.col.ast.{RangeSet, TInt, TSet, Type}
import vct.col.ast.ops.RangeSetOps
import vct.col.print._

trait RangeSetImpl[G] extends RangeSetOps[G] {
  this: RangeSet[G] =>
  override def t: Type[G] = TSet(TInt())

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("{") <> from <+> ".." <+> to <> "}"
}

package vct.col.ast.unsorted

import vct.col.ast.TEnum
import vct.col.ast.ops.TEnumOps
import vct.col.print._

trait TEnumImpl[G] extends TEnumOps[G] {
  this: TEnum[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

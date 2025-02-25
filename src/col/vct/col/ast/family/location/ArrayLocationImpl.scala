package vct.col.ast.family.location

import vct.col.ast.ArrayLocation
import vct.col.print._
import vct.col.ast.ops.ArrayLocationOps

trait ArrayLocationImpl[G] extends ArrayLocationOps[G] {
  this: ArrayLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(array.bind(Precedence.POSTFIX) <> "[" <> Doc.arg(subscript) <> "]")
}

package vct.col.ast.unsorted

import vct.col.ast.EnumConstant
import vct.col.ast.ops.{EnumConstantOps, EnumConstantFamilyOps}
import vct.col.print._

trait EnumConstantImpl[G]
    extends EnumConstantOps[G] with EnumConstantFamilyOps[G] {
  this: EnumConstant[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

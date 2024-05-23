package vct.col.ast.unsorted

import vct.col.ast.BipPortType
import vct.col.ast.ops.BipPortTypeFamilyOps
import vct.col.print._

trait BipPortTypeImpl[G] extends BipPortTypeFamilyOps[G] {
  this: BipPortType[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

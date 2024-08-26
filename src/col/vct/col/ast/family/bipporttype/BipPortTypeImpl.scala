package vct.col.ast.family.bipporttype

import vct.col.ast.BipPortType
import vct.col.ast.ops.BipPortTypeFamilyOps

trait BipPortTypeImpl[G] extends BipPortTypeFamilyOps[G] {
  this: BipPortType[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

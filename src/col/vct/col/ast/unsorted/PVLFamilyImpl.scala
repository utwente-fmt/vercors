package vct.col.ast.unsorted

import vct.col.ast.PVLFamily
import vct.col.ast.ops.{PVLFamilyOps, PVLFamilyFamilyOps}
import vct.col.print._

trait PVLFamilyImpl[G] extends PVLFamilyOps[G] with PVLFamilyFamilyOps[G] {
  this: PVLFamily[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

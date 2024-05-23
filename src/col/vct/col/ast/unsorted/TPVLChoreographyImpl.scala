package vct.col.ast.unsorted

import vct.col.ast.TPVLChoreography
import vct.col.print._
import vct.col.ast.ops.TPVLChoreographyOps

trait TPVLChoreographyImpl[G] extends TPVLChoreographyOps[G] {
  this: TPVLChoreography[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

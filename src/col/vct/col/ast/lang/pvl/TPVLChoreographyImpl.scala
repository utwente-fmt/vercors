package vct.col.ast.lang.pvl

import vct.col.ast.TPVLChoreography
import vct.col.ast.ops.TPVLChoreographyOps

trait TPVLChoreographyImpl[G] extends TPVLChoreographyOps[G] {
  this: TPVLChoreography[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

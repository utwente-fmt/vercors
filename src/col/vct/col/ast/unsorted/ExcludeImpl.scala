package vct.col.ast.unsorted

import vct.col.ast.{Exclude, FilterMode, Include}
import vct.col.ast.ops.ExcludeOps
import vct.col.print._

trait ExcludeImpl[G] extends ExcludeOps[G] {
  this: Exclude[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

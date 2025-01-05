package vct.col.ast.unsorted

import vct.col.ast.{Exclude, FilterMode, Include}
import vct.col.ast.ops.IncludeOps
import vct.col.print._

trait IncludeImpl[G] extends IncludeOps[G] {
  this: Include[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

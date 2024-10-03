package vct.col.ast.unsorted

import vct.col.ast.CoerceBetweenUniqueClass
import vct.col.ast.ops.CoerceBetweenUniqueClassOps
import vct.col.print._

trait CoerceBetweenUniqueClassImpl[G] extends CoerceBetweenUniqueClassOps[G] { this: CoerceBetweenUniqueClass[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

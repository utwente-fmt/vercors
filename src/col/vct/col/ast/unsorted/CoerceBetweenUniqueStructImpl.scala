package vct.col.ast.unsorted

import vct.col.ast.CoerceBetweenUniqueStruct
import vct.col.ast.ops.CoerceBetweenUniqueStructOps
import vct.col.print._

trait CoerceBetweenUniqueStructImpl[G] extends CoerceBetweenUniqueStructOps[G] { this: CoerceBetweenUniqueStruct[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

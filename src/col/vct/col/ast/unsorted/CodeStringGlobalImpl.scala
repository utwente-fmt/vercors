package vct.col.ast.unsorted

import vct.col.ast.CodeStringGlobal
import vct.col.ast.ops.CodeStringGlobalOps
import vct.col.print._

trait CodeStringGlobalImpl[G] extends CodeStringGlobalOps[G] { this: CodeStringGlobal[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

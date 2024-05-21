package vct.col.ast.unsorted

import vct.col.ast.CodeStringClass
import vct.col.ast.ops.CodeStringClassOps
import vct.col.print._

trait CodeStringClassImpl[G] extends CodeStringClassOps[G] { this: CodeStringClass[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

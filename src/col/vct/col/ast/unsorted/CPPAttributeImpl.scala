package vct.col.ast.unsorted

import vct.col.ast.CPPAttribute
import vct.col.ast.ops.{CPPAttributeOps, CPPAttributeFamilyOps}
import vct.col.print._

trait CPPAttributeImpl[G] extends CPPAttributeOps[G] with CPPAttributeFamilyOps[G] { this: CPPAttribute[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

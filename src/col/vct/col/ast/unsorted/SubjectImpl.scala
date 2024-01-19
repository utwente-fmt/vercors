package vct.col.ast.unsorted

import vct.col.ast.Subject
import vct.col.ast.ops.SubjectFamilyOps
import vct.col.print._

trait SubjectImpl[G] extends SubjectFamilyOps[G] { this: Subject[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

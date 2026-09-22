package vct.col.ast.lang.Isar

import vct.col.ast.IsarCommand
import vct.col.ast.ops.IsarCommandFamilyOps

trait IsarCommandImpl[G] extends IsarCommandFamilyOps[G] {
  this: IsarCommand[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}

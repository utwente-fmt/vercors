package vct.col.ast.family.access

import vct.col.ast.{Access, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{AccessOps, AccessFamilyOps}

trait AccessImpl[G] extends AccessOps[G] with AccessFamilyOps[G] { this: Access[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    subject.show <> "." <> ctx.name(field)
  }
}

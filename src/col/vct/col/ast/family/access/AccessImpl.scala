package vct.col.ast.family.access

import vct.col.ast.{Access, Type}
import vct.col.print.{Ctx, Doc, Text}

trait AccessImpl[G] { this: Access[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    subject.show <> "." <> field.decl.o.getPreferredNameOrElse()
  }
}

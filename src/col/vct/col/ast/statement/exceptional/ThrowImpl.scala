package vct.col.ast.statement.exceptional

import vct.col.ast.Throw
import vct.col.print.{Ctx, Doc, Text}

trait ThrowImpl[G] {
  this: Throw[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("throw") <+> obj <> ";"
}

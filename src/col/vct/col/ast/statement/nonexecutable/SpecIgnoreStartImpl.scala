package vct.col.ast.statement.nonexecutable

import vct.col.ast.SpecIgnoreStart
import vct.col.print.{Ctx, Doc, Text}

trait SpecIgnoreStartImpl[G] {
  this: SpecIgnoreStart[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Text("spec_ignore {"))
}

package vct.col.ast.statement.nonexecutable

import vct.col.ast.SpecIgnoreEnd
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SpecIgnoreEndOps

trait SpecIgnoreEndImpl[G] extends SpecIgnoreEndOps[G] { this: SpecIgnoreEnd[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Text("spec_ignore }"))
}
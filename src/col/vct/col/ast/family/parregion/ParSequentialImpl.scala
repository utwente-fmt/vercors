package vct.col.ast.family.parregion

import vct.col.ast.ParSequential
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.ParSequentialOps

trait ParSequentialImpl[G] extends ParSequentialOps[G] {
  this: ParSequential[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("sequential") <+> "{" <>> Doc.stack(regions) <+/> "}"
}

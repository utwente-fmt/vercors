package vct.col.ast.family.parregion

import vct.col.ast.ParSequential
import vct.col.print.{Ctx, Doc, Text}

trait ParSequentialImpl[G] {
  this: ParSequential[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("sequential") <+> "{" <>> Doc.stack(regions) <+/> "}"
}

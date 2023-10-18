package vct.col.ast.family.parregion

import vct.col.ast.ParParallel
import vct.col.print._

trait ParParallelImpl[G] {
  this: ParParallel[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("parallel") <+> "{" <>> Doc.stack(regions) <+/> "}"
}

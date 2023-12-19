package vct.col.ast.family.parregion

import vct.col.ast.ParParallel
import vct.col.print._
import vct.col.ast.ops.ParParallelOps

trait ParParallelImpl[G] extends ParParallelOps[G] { this: ParParallel[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("parallel") <+> "{" <>> Doc.stack(regions) <+/> "}"
}
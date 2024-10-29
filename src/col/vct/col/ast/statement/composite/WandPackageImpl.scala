package vct.col.ast.statement.composite

import vct.col.ast.WandPackage
import vct.col.print._
import vct.col.ast.ops.WandPackageOps

trait WandPackageImpl[G] extends WandPackageOps[G] {
  this: WandPackage[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("package") <>> res) <+> proof.layoutAsBlock
}

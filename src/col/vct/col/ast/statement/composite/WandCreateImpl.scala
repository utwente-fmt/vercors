package vct.col.ast.statement.composite

import vct.col.ast.WandPackage
import vct.col.print._

trait WandCreateImpl[G] { this: WandPackage[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("package") <>> res) <+> proof.layoutAsBlock
}
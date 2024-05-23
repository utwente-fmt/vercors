package vct.col.ast.statement.composite

import vct.col.ast.ModelDo
import vct.col.print._
import vct.col.ast.ops.ModelDoOps

trait ModelDoImpl[G] extends ModelDoOps[G] { this: ModelDo[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("action(") <> Doc.args(Seq(model, perm, after, action)) <> ")") <+> impl.layoutAsBlock
}
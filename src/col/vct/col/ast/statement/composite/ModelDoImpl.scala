package vct.col.ast.statement.composite

import vct.col.ast.ModelDo
import vct.col.print._

trait ModelDoImpl[G] { this: ModelDo[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("action(") <> Doc.args(Seq(model, perm, after, action)) <> ")") <+> impl.layoutAsBlock
}
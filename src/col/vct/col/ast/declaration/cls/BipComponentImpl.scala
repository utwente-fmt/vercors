package vct.col.ast.declaration.cls

import vct.col.ast.BipComponent
import vct.col.print._

trait BipComponentImpl[G] {
  this: BipComponent[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Text("/*"),
      Text("javaBipComponent(") <> Doc.args(Seq(
        Text("name =") <+> ctx.name(this),
        Text("state =") <+> ctx.name(initial),
        Text("invariant =") <+> invariant,
      )) <> ")",
      Text("*/"),
    ))
}

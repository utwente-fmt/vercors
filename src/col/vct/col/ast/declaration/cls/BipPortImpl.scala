package vct.col.ast.declaration.cls

import vct.col.ast.BipPort
import vct.col.print._

trait BipPortImpl[G] {
  this: BipPort[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(
      Seq(Text("/*"), t.show <+> "javaBipPort" <+> ctx.name(this), Text("*/"))
    )
}

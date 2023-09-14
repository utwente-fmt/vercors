package vct.col.ast.statement.composite

import vct.col.ast.{Declaration, RangedFor}
import vct.col.print._

trait RangedForImpl[G] { this: RangedFor[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(iter.variable)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Group(Text("for") <> "(" <> Doc.arg(iter) <> ")"),
    )) <+> body.layoutAsBlock
}

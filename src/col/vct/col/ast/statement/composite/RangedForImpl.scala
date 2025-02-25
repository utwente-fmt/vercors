package vct.col.ast.statement.composite

import vct.col.ast.{Declaration, RangedFor}
import vct.col.print._
import vct.col.ast.ops.RangedForOps

trait RangedForImpl[G] extends RangedForOps[G] {
  this: RangedFor[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(iter.variable)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(
      Seq(contract.show, Group(Text("for") <> "(" <> Doc.arg(iter) <> ")"))
    ) <+> body.layoutAsBlock
}

package vct.col.ast.statement.composite

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, VecBlock}
import vct.col.print._

trait VecBlockImpl[G] extends Declarator[G] {
  this: VecBlock[G] =>
  override def declarations: Seq[Declaration[G]] = iters.map(_.variable)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Group(Text("vec") <> "(" <> Doc.args(iters) <> ")"),
      DocUtil.clauses("requires", requires),
      DocUtil.clauses("ensures", ensures),
    )) <+> content.layoutAsBlock
}

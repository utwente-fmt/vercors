package vct.col.ast.statement.composite

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, ParInvariant}
import vct.col.print.{Ctx, Doc, Group, Text}

trait ParInvariantImpl[G] extends Declarator[G] {
  this: ParInvariant[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(decl)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("invariant") <+> ctx.name(decl) <> "(" <> Doc.arg(inv) <> ")"
    ) <+> content.layoutAsBlock
}

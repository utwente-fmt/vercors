package vct.col.ast.family.signals

import vct.col.ast.{Declaration, SignalsClause}
import vct.col.ast.util.Declarator
import vct.col.print.{Ctx, Doc, Text, Group}

trait SignalsClauseImpl[G] extends Declarator[G] {
  this: SignalsClause[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(binding)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("signals") <+> "(" <> binding <> ")" <>> assn)
}

package vct.col.ast.family.signals

import vct.col.ast.{Declaration, SignalsClause, TResource}
import vct.col.ast.util.Declarator
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.{SignalsClauseFamilyOps, SignalsClauseOps}
import vct.col.check.{CheckContext, CheckError, TypeError}
import vct.col.typerules.CoercionUtils

trait SignalsClauseImpl[G]
    extends Declarator[G]
    with SignalsClauseOps[G]
    with SignalsClauseFamilyOps[G] {
  this: SignalsClause[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(binding)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    CoercionUtils.getCoercion(assn.t, TResource()) match {
      case Some(_) => Nil
      case None => Seq(TypeError(assn, TResource()))
    }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("signals") <+> "(" <> binding <> ")" <>> assn)
}

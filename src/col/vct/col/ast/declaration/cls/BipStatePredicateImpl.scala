package vct.col.ast.declaration.cls

import vct.col.ast.BipStatePredicate
import vct.col.print.{Ctx, Doc, Text, Group}
import vct.col.ast.ops.BipStatePredicateOps

trait BipStatePredicateImpl[G] extends BipStatePredicateOps[G] {
  this: BipStatePredicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("javaBipStatePredicate") <+> ctx.name(this) <+> "{" <>> expr <+/> "}"
    )
}

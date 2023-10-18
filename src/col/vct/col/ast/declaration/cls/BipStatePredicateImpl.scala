package vct.col.ast.declaration.cls

import vct.col.ast.BipStatePredicate
import vct.col.print.{Ctx, Doc, Text, Group}

trait BipStatePredicateImpl[G] {
  this: BipStatePredicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("javaBipStatePredicate") <+> ctx.name(this) <+> "{" <>> expr <+/> "}"
    )
}

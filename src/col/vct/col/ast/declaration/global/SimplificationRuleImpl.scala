package vct.col.ast.declaration.global

import vct.col.ast.SimplificationRule
import vct.col.print.{Ctx, Doc, Text, Nest, Line}

trait SimplificationRuleImpl[G] { this: SimplificationRule[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("axiom") <+> ctx.name(this) <+> "{" <>>
      { axiom.show } <+/>
    "}"
}
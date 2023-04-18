package vct.col.ast.declaration.global

import vct.col.ast.SimplificationRule
import vct.col.print._

trait SimplificationRuleImpl[G] { this: SimplificationRule[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("axiom") <+> ctx.name(this) <+> "{" <>>
      { axiom.show } <+/>
    "}")
}
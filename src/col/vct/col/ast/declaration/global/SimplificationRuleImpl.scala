package vct.col.ast.declaration.global

import vct.col.ast.SimplificationRule
import vct.col.print._
import vct.col.ast.ops.SimplificationRuleOps

trait SimplificationRuleImpl[G] extends SimplificationRuleOps[G] { this: SimplificationRule[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("axiom") <+> ctx.name(this) <+> "{" <>>
      { axiom.show } <+/>
    "}")
}
package vct.col.ast.family.itervariable

import vct.col.ast.IterVariable
import vct.col.print._
import vct.col.ast.ops.{IterVariableOps, IterVariableFamilyOps}

trait IterVariableImpl[G]
    extends IterVariableOps[G] with IterVariableFamilyOps[G] {
  this: IterVariable[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(variable.show <+> "=" <>> Group(from.show <+> ".." <+/> to))
}

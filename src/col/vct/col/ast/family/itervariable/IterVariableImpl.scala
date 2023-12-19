package vct.col.ast.family.itervariable

import vct.col.ast.IterVariable
import vct.col.print._

trait IterVariableImpl[G] { this: IterVariable[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(variable.show <+> "=" <>> Group(from.show <+> ".." <+/> to))
}
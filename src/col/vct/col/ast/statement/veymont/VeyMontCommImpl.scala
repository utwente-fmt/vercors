package vct.col.ast.statement.veymont

import vct.col.ast.VeyMontCommExpression
import vct.col.print.{Ctx, Doc}

trait VeyMontCommImpl[G] { this: VeyMontCommExpression[G] =>
  override def layout(implicit ctx: Ctx): Doc = assign.show
}

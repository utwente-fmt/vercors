package vct.col.ast.statement.veymont

import vct.col.ast.VeyMontAssignExpression
import vct.col.print._

trait VeyMontAssignExpressionImpl[G] {
  this: VeyMontAssignExpression[G] =>
  override def layout(implicit ctx: Ctx): Doc = assign.show
}

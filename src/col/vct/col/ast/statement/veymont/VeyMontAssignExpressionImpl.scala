package vct.col.ast.statement.veymont

import vct.col.ast.VeyMontAssignExpression
import vct.col.print._
import vct.col.ast.ops.VeyMontAssignExpressionOps

trait VeyMontAssignExpressionImpl[G] extends VeyMontAssignExpressionOps[G] { this: VeyMontAssignExpression[G] =>
  override def layout(implicit ctx: Ctx): Doc = assign.show
}

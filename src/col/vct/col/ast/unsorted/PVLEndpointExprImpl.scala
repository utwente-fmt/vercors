package vct.col.ast.unsorted

import vct.col.ast.{PVLEndpointExpr, Type}
import vct.col.ast.ops.PVLEndpointExprOps
import vct.col.print._

trait PVLEndpointExprImpl[G] extends PVLEndpointExprOps[G] {
  this: PVLEndpointExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("(\\[") <> endpoint <> "]" <+> expr <> ")"

  def t: Type[G] = expr.t
}

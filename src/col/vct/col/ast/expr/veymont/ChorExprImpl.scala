package vct.col.ast.expr.veymont

import vct.col.ast.ops.ChorExprOps
import vct.col.ast.{ChorExpr, Type}
import vct.col.print._

trait ChorExprImpl[G] extends ChorExprOps[G] {
  this: ChorExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("(\\chor") <+> expr <> ")"
  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = expr.t
}

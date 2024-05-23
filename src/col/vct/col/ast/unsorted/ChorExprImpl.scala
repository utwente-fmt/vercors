package vct.col.ast.unsorted

import vct.col.ast.{ChorExpr, TBool, Type}
import vct.col.ast.ops.ChorExprOps
import vct.col.print._

trait ChorExprImpl[G] extends ChorExprOps[G] {
  this: ChorExpr[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("(\\chor") <+> expr <> ")"

  override def t: Type[G] = expr.t
}

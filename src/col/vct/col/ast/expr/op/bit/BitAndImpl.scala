package vct.col.ast.expr.op.bit

import vct.col.ast.{BitAnd, TInt, Type}
import vct.col.print._
import vct.col.ast.ops.BitAndOps

trait BitAndImpl[G] extends BitAndOps[G] {
  this: BitAnd[G] =>
  override def t: Type[G] = getIntType

  override def precedence: Int = Precedence.BIT_AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&", right)
}

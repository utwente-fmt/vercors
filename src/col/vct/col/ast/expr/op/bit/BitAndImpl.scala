package vct.col.ast.expr.op.bit

import vct.col.ast.{BitAnd, TInt, Type}
import vct.col.print._

trait BitAndImpl[G] { this: BitAnd[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.BIT_AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "&", right)
}
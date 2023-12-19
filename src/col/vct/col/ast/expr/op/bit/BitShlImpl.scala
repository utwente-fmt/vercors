package vct.col.ast.expr.op.bit

import vct.col.ast.{BitShl, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait BitShlImpl[G] { this: BitShl[G] =>
  override def t: Type[G] = getIntType

  override def precedence: Int = Precedence.SHIFT
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "<<", right)
}
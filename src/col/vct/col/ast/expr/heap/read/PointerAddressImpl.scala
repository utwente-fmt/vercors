package vct.col.ast.expr.heap.read

import vct.col.ast.ops.PointerAddressOps
import vct.col.ast.{PointerAddress, TInt, Type}
import vct.col.print._

trait PointerAddressImpl[G] extends PointerAddressOps[G] {
  this: PointerAddress[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_address(") <> pointer <> ")"
}

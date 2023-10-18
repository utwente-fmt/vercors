package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerBlockLength, TInt, Type}
import vct.col.print._

trait PointerBlockLengthImpl[G] {
  this: PointerBlockLength[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_block_length(") <> pointer <> ")"
}

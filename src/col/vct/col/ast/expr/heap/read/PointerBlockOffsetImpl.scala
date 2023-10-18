package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerBlockOffset, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait PointerBlockOffsetImpl[G] {
  this: PointerBlockOffset[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_block_offset(") <> pointer <> ")"
}

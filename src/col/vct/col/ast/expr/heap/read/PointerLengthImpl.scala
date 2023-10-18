package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerLength, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait PointerLengthImpl[G] {
  this: PointerLength[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_length(") <> pointer <> ")"
}
